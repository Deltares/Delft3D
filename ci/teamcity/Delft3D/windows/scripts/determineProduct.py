import sys

product = sys.argv[1]
branch = sys.argv[2]
is_default = sys.argv[3]
vcs_number = sys.argv[4]
pr_source_branch = sys.argv[5] if len(sys.argv) > 5 else ""

if product == "auto-select":
    if "pull" in branch:
        if pr_source_branch.startswith("revert-") or pr_source_branch.startswith("dependabot/"):
            product = "tc"
        else:
            product = pr_source_branch.split("/")[0]
    else:
        product = branch.split("/")[0]
    if is_default == "true":
        product = "all"

    print(f"##teamcity[setParameter name='product' value='{product}-testbench']")
    print(f"##teamcity[buildNumber '{product}-testbench: {vcs_number}']")
