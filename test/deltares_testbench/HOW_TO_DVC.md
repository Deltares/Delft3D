# Managing test case data with `dvc`

## Table of contents
- [Introduction](#introduction)
- [Why we version the test case data](#why-we-version-the-test-case-data)
- [DVC: Data Version Control](#dvc-data-version-control)
- [HOW TO: Download test case data](#how-to-download-test-case-data)
- [HOW TO: Update test case data](#how-to-update-test-case-data)
    - [Updating a single test case](#updating-a-single-test-case)
    - [Updating multiple test cases](#updating-multiple-test-cases)
- [HOW TO: Add test case data](#how-to-add-test-case-data)
- [HOW TO: Clean up the test case data](#how-to-clean-up-the-test-case-data-from-your-hard-drive)
- [HOW TO: See the changes to the test case data between different commits](#how-to-see-the-changes-to-the-test-case-data-between-different-commits)

## Introduction
We have a large collection of test cases that we can run with `TestBench.py`. Each
test case has input files, reference files and documentation. To run test cases with 
`TestBench.py` a config file is required. This config file contains information on how
to run the test cases, and where to find the test case data (You can find plenty of 
examples in the [configs](./configs/) directory).

This document describes how to perform some common tasks with test case data,
such as:
- Downloading the input files of one or several test cases.
- Updating the input files or reference files of one or several test cases.
- Uploading input data for an entirely new test case.

This document does not contain any information about config files for 
`TestBench.py`. If you need help writing or updating a config file, you can take
a look at the [`TestBench.py documentation`](./doc/readme-testbench.pdf)

## Why we version the test case data
Running a test case with `TestBench.py` performs the following steps:
1. Download the test case input files.
2. Download the test case reference (output) files.
3. Run a program (or several programs) using the input files. This produces output 
files.
4. Compare the output files produced in the previous step with the reference
files.
5. The test fails if programs fail to generate the expected output files, or if the
differences between the output and the reference files are not within tolerance.

`TestBench.py` allows the developers to remain confident that the software still 
produces correct output as changes to the software are introduced. We don't allow 
changes to the software that produce differences above tolerances in any test case.
The trouble is that we need to store the test case input and reference files somewhere.

Just like our code, our test case data changes over time.
New functionality requires new test cases. So it should be possible to create new
test cases as new features are introduced. Sometimes either the input files or 
reference files for an existing test case need to be updated. For instance, the input
files may be using deprecated or obsolete functionality. 
A change in the software may produce differences above tolerances. Sometimes the 
differences are actually regarded as an improvement. In that case the reference files 
need to be updated. Over time, a history of test case data emerges. We want to be able to
roll back to a specific version of the software and run the test cases with the test
case data as it was at the time that version of the software was made. Furthermore,
we want developers to be able to make changes to test case data simultaneously without 
unnecessary conflicts.

We already version our software with `git`. Ideally, we
could track all of our test case data with `git` as well. This way, our test
case data would be versioned automatically and it would always be up-to-date 
with the software in the repository. Unfortunately, this is infeasible for a 
couple of reasons:
1. Some test cases contain input files larger than the file size limit for the git
remote (GitHub).
2. Our GitHub repository is publicly accessible. But some of the test cases that
we run should not be publically accessible.
3. The number of files and total size of the test case data are both quite large. This
makes it somewhat impracticle to clone the repository, unless you explicitly exclude
the test case data. Our build agents clone the repository all the time, and most of the
time they need only a tiny percentage of the test case data.

We need to store the test case data somewhere where it is not
publicly accessible, and in a place that can handle many (large) files. 
The test case data should be versioned, in such a way that we can easily link a commit to 
the correct test case data. It should be possible to download test case data only when it
is needed. Furthermore, it should not be difficult to create new test case data and
update existing test case data. Updates to the test case data can be done in a branch, and 
multiple developers should be able to make changes to the test case data simultaneously in 
different branches, without unnecessary conflicts.

## DVC: Data Version Control

[DVC](https://dvc.org/) is the tool we've chosen to version our test case data. It replaces
the "MinIO tools" that we developed in-house. If you're familiar with the MinIO tools, you'll
know that each test case in the testbench config files contains a "version timestamp". The
testbench is able to "rewind" time to this timestamp to recover the files as they were at
that exact time. The timestamp covers all of the case input, reference and documentation files.
So if someone updates just the documentation of a test case in branch `A`, then someone else who
only updates the references of the same case in branch `B` will get a merge conflict on the 
timestamp.

With DVC we no longer track the version of the test case data with a timestamp in the test bench
config files. Instead we track the versions of the test case data in `.dvc` files, that are
automatically generated by the `dvc` command line tool. We never need to edit these files by
hand. In fact, this is discouraged. In the folder `test/deltares_testbench/data/cases`, we now 
have a folder structure containing all of our test cases. For each test case we have at least 
three `.dvc` files: One for the test case input, one for the references (two if the reference 
files differ between Windows and Linux), and one for the test case documentation. Example for a 
single test case:

```
test/deltares_testbench/data/cases/e02_dflowfm/f040_subsidence/c01_uniform_ibed_1/
├── doc.dvc
├── input.dvc
└── reference_win64.dvc
```

Each `.dvc` file tracks the content of a directory of files: The `doc/`, `input/` and `reference_win64/`
directories. Notice that the directory name is shared with the `.dvc` file. In the directory listing
above, these directories are not shown. You can download the directories by invoking 
`dvc pull doc.dvc input.dvc reference_win64.dvc`. Then the listing will look like this:

```
test/deltares_testbench/data/cases/e02_dflowfm/f040_subsidence/c01_uniform_ibed_1/
├── doc/
├── doc.dvc
├── input/
├── input.dvc
├── reference_win64/
└── reference_win64.dvc
```

The split between the case input, references and documentation was made so that people can,
for example, update the case input and the documentation simultaneously without merge conflicts.

The `.dvc` files are small text files that contain a "hash" of the content of the  data.
DVC maintains an index from these hashes to the content of the files. This index is stored
remotely, in our case in a MinIO bucket called `delft3d-testbench`. As you use `dvc` to download 
files, entries of the index will be cached on your hard drive as well (in the `.dvc` folder in 
the root of the Delft3D repo). Notice that we end up with a single hash for an entire directory 
of files. How DVC handles directories is by creating a small file, containing a mapping from 
directory entries to hashes. The hash of the content this "directory file" is then used as the 
hash for the entire directory. You can recognise directory hashes by the `.dir` postfix in the 
`.dvc` files.

All of the `.dvc` files under `data/cases` are tracked by `git`. In the past, this entire directory
was ignored by `git`, but we now have an exception in our `.gitignore` file specifically for `.dvc`
files.

## HOW TO: Download test case data
Use `dvc pull <path-to-dvc-file>` to download test case data for existing test cases.

The following command downloads *only the test case input files* for the test case `e02_f040_c01_uniform_ibed_1`.
```bash
# From directory "test/deltares_testbench/"
dvc pull data/cases/e02_dflowfm/f040_subsidence/c01_uniform_ibed_1/input.dvc
```

To avoid having to write long paths, you can `cd` into the directory containing the `.dvc` files.
You can specify multiple `.dvc` files with `dvc pull`. In addition to the input files, the
following command downloads the references and the documentation files:

```bash
# From directory "test/deltares_testbench/"
cd data/cases/e02_dflowfm/f040_subsidence/c01_uniform_ibed_1
dvc pull reference_win64.dvc doc.dvc
```

After running both of the commands above, a directory listing confirms that the case input, 
references and documentation files have been downloaded.
```
test/deltares_testbench/data/cases/e02_dflowfm/f040_subsidence/c01_uniform_ibed_1/
├── doc/
├── doc.dvc
├── input/
├── input.dvc
├── reference_win64/
└── reference_win64.dvc
```

Instead of writing down the individual file names, you can use "glob patterns" to specify which 
`.dvc` files to pull. In `bash`, support for glob patterns is built in. But for other shells the
command looks a bit different. For Windows users, `powershell` is able to expand glob patterns
with the `ls` command. Unfortunately, `cmd.exe` lacks globbing functionality. The following
command again downloads the case input, references and documentation files for the test case `e02_f040_c01_uniform_ibed_1`:

***bash***
```bash
# From directory "test/deltares_testbench/"
dvc pull data/cases/e02_dflowfm/f040_subsidence/c01_uniform_ibed_1/*.dvc
```

***powershell***
```powershell
# From directory "test/deltares_testbench/"
dvc pull @(ls .\data\cases\e02_dflowfm\f040_subsidence\c01_uniform_ibed_1\*.dvc)
```

Glob patterns also come in handy when you want to download test case data for many test
cases in one command. The following command downloads the case input of all of the test
cases in the `f040_subsidence` functionality group:

***bash***
```bash
# From directory "test/deltares_testbench/"
dvc pull data/cases/e02_dflowfm/f040_subsidence/c*/input.dvc
```

***powershell***
```powershell
# From directory "test/deltares_testbench/"
dvc pull @(ls .\data\cases\e02_dflowfm\f040_subsidence\c*\input.dvc)
```

## HOW TO: Update test case data
Updating the test case data means changing the content of the `input`, `doc` and/or 
`reference_{win,lnx}64` directories. This changes their respective content hashes,
leading to a change in the corresponding `.dvc` files. Let's first see how to update the
test case data of a single test case. Then the case of updating multiple test cases is a 
straightforward extension of this workflow.

### Updating a single test case

The following steps update the content of the `.dvc` files, which are tracked by `git`. Make 
sure that you are working in a separate branch. If you're working on the `main` branch you 
will not be able to push your changes.

Let's update the case input files of the `e02_f040_c01_uniform_ibed_1` test case.
Make sure that we have "current" test case data checked out.

```bash
# From "test/deltares_testbench/"
cd data/cases/e02_dflowfm/f040_subsidence/c01_uniform_ibed_1/
dvc pull input.dvc
```

Now, we need to make a change. Let's say we want to rename `dimr.xml` to `dimr_config.xml`, in
a hypothetical effort to unify all of the test cases to use the `dimr_config.xml` name.
```bash
mv input/dimr.xml input/dimr_config.xml
```

Now that we've changed the test case input data. We have to recompute the hash in `input.dvc`.
This is accomplished with the `dvc commit <path-to-dvc-file>` command.
```bash
dvc commit input.dvc
```

Note: `dvc` will prompt you first to ensure that you want to do this. If you don't want to 
be prompted interactively you can add the `-f|--force` flag.

You can check with `git status` that the `input.dvc` file has been modified. A `git diff` should
reveal that the hash in `input.dvc` was updated.

The test case data was only updated in your "local" index (in the `.dvc` folder in the `Delft3D`
repository root). The latest version of the data still needs to be pushed to the "remote" index
(the `delft3d-testbench` bucket). Only then can other people download the updated test case data 
from your branch. The following command pushes the updated data to the remote:
```bash
dvc push input.dvc
```

You can now stage and `git commit` the updated `input.dvc` file. You might see the
following message during staging. As stated in the message, you can add the `-f|--force` flag to 
force staging the `input.dvc` file.
```
The following paths are ignored by one of your .gitignore files:
test/deltares_testbench/data/cases
hint: Use -f if you really want to add them.
hint: Turn this message off by running
hint: "git config advice.addIgnoredFile false"
```

And that's it for updating a single test case. You can verify that the data has been pushed to the
remote by removing the `input` folder and running `dvc pull input.dvc` again. You should get the
test case input data, with `dimr_config.xml` instead of `dimr.xml` this time. If you want the
change to land in `main`, `git push` your changes and create a pull request in GitHub.

### Updating multiple test cases

You can follow the steps in the [previous subsection](#updating-a-single-test-case) one-by-one for
each test case. But you can also update multiple test cases in one invocation of `dvc`. 
For example by using glob patterns. The steps are very similar. First make sure you've downloaded
the test case data for every test case you want to change with `dvc pull`. Let's say every test 
case in the "f040_subsidence" functionality group:

***bash***
```bash
dvc pull data/cases/e02_dflowfm/f040_subsidence/c*/input.dvc
```

***powershell***
```powershell
dvc pull @(ls data/cases/e02_dflowfm/f040_subsidence/c*/input.dvc)
```

Then update the files for each case in bulk. You can use whatever tool you have at your disposal
to do this. After you're done, `dvc commit` your changes to update the relevant `.dvc` files:

***bash***
```bash
dvc commit data/cases/e02_dflowfm/f040_subsidence/c*/input.dvc  # Add -f|--force to avoid prompts
```

***powershell***
```powershell
dvc commit @(ls data/cases/e02_dflowfm/f040_subsidence/c*/input.dvc)  # Add -f|--force to avoid prompts
```

This should update all of the `input.dvc` files whose contents have changed. Remember to push the
updated test case files to the remote:

***bash***
```bash
dvc push data/cases/e02_dflowfm/f040_subsidence/c*/input.dvc
```

***powershell***
```powershell
dvc push @(ls data/cases/e02_dflowfm/f040_subsidence/c*/input.dvc)
```

Now you're ready to stage and `git commit` the `input.dvc` files.

Note: If the test cases you need to update don't fit neatly in a single glob pattern, you can
also store a list of paths to `.dvc` files in a variable. Then you can use your shell's variable
expansion to make the commands above work for the exact set of test cases you're working on.

## HOW TO: Add test case data
To add a test case data you need to create a new subdirectory in the directory tree inside
`test/deltares_testbench/data/cases`. Before you can start you will need to know which
"engine" and "functionality group" the test case belongs to. So you know which subdirectory
to add the test case data to. Then you assign the test case a "case number" that isn't already
in use, and think of a good name to describe the test case. Say you're creating a new test case in
`e02_dflowfm/f040_subsidence/c98_new_test_case/`. This directory needs at most four subfolders:
- input/
- reference_win64/
- reference_lnx64/  (Only required if the references are platform dependent)
- doc/

Technically you can add more folders and name them whatever you want. But the first three folders
(everything but `doc/`) have special significance to `TestBench.py`. If this test case is supposed
to be run in the test bench, then make sure you conform to this naming convention. The `doc/` folder
has special significance for building the functionality documentation. Please stick to the naming 
convention.

You need to move the model input, reference files and the documentation to the right directories.
Once you're happy with the contents of these folders, you can generate a `.dvc` file for each of
them individually:

```bash
# From "test/deltares_testbench/data/cases/e02_dflowfm/f040_subsidence/c98_new_test_case"
dvc add input
dvc add reference_win64
dvc add reference_lnx64
dvc add doc
```

Or you can add them all in one go, if that's what you prefer.
```bash
dvc add input reference_win64 reference_lnx64 doc
```

The `dvc add` command will only generate the `.dvc` files, with the content hash of the directories
inside. In addition it will add the files to your "local" index. But, as was the case with updating
test cases, you still need to push the files to the "remote" index.

```bash
dvc push input.dvc
dvc push reference_win64.dvc
dvc push reference_lnx64.dvc
dvc push doc.dvc
```

Or again, in one invocation:
```bash
dvc push input.dvc reference_win64.dvc reference_lnx64.dvc doc.dvc
```

Now you're ready to stage and `git commit` the `.dvc` files to your branch. If you `git push` your
branch and create a pull request on GitHub, people will be able to checkout your branch and `dvc 
pull` your new test case.

## HOW TO: Clean up the test case data from your hard drive
Your `test/deltares_testbench/data/cases` directory might take up a lot of disk space after a 
while. Maybe because you've had to download many test cases. Before, you would be able to simply
remove the `cases` directory. But you can't do that anymore, because there are now these `.dvc`
files inside that are tracked by `git`. Fortunately, you can throw away all files inside the
`cases` directory, except for the `.dvc` files with `git clean`:

```bash
# From "test/deltares_testbench"
# Cleans up all files ignored by git inside the "data/cases" directory.
git clean -fdx data/cases/
```

If you wish to inspect the files this command would delete, without actually deleting anything,
use the `-n` flag:

```bash
# Don't delete anything, this is just a "dry-run".
git clean -fdxn data/cases/  
```
Of course you can also specify a subdirectory of `data/cases`, to delete only files from that
subdirectory.

To save some more disk space, you can also remove the `.dvc/cache` directory (in the root of the
Delft3D repository). This is your local copy of the DVC index. It can be safely removed because 
DVC can recover it by pulling files from the remote index.

## HOW TO: See the changes to the test case data between different commits

> [!WARNING]
> Users of `cmd.exe` be warned: We have noticed that the `dvc diff` command does not
> generate correct results when invoked with `cmd.exe`. For now we advise Windows
> users to use `powershell` instead when working with `dvc`. A work-around for people
> who would like to keep using `cmd.exe`: You can invoke the commands through
> powershell like this: `powershell -c "dvc diff [arguments...]"`

You can use `dvc diff` to inspect the changed files in the test case data between
any two `git` commits. First make sure you have the most recent `git` commits stored
in your local `git` index:

```bash
git fetch
```

Then, to inspect the changes between `main` and `my-branch`:

```bash
# Calculate the changed files in the "input" folder of a test case between two branches.
dvc diff main my-branch --targets path/to/my/input.dvc
```

`dvc diff` understands branch names, tags, but also `git` commit ids. Any kind of
`git` [revision](https://git-scm.com/docs/gitrevisions) should work, in fact.

```bash
# Calculate the changed documentation files between the previous commit and the current commit.
dvc diff HEAD^ HEAD --targets path/to/my/doc.dvc
```

Unfortunately, `dvc diff` only lists the names of the files that have been modified. Currently
it can't generate diffs of the actual file contents.
