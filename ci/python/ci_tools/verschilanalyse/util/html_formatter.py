import textwrap
from typing import ClassVar, Iterable, Iterator

from ci_tools.verschilanalyse.util.excel_exporter import LogComparison
from ci_tools.verschilanalyse.util.verschilanalyse_comparison import VerschilanalyseComparison
from ci_tools.verschilanalyse.util.verschillentool import (
    OutputType,
    VariableRegistry,
    VerschillentoolOutput,
    Statistics,
)


class HtmlFormatter:
    """Contains the HTML formatting code to format the weekly verschilanalyse email."""

    STYLESHEET: ClassVar[str] = textwrap.dedent(
        """
        table {
            table-layout: fixed;
            border-collapse: collapse;
            border: 1px solid black;
        }

        th, td {
            border: 1px solid black;
            padding: 4px;
        }

        td.align-right {
            text-align: right;
        }
        """
    ).strip()

    TEMPLATE: ClassVar[str] = textwrap.dedent(
        """
        <html>
            <head>
                <style>
                    {style}
                </style>
            </head>
            <body>
                <h2>New weekly verschilanalyse</h2>
                <p>
                    A new weekly automated verschilanalyse has been completed.
                    The current output and the reference output are archived in our MinIO bucket under the following
                    prefixes:
                </p>
                <ul id="prefix-list">
                    <li>Current output: {current_prefix}</li>
                    <li>Reference output: {reference_prefix}</li>
                </ul>
                <h3>Verschillentool output</h3>
                <p>
                    The verschillentool compared the current output files to the
                    reference output files, and found the following results:
                </p>
                <h3>His-file comparison results</h3>
                {his_tolerance_list}
                <h3>Map-file comparison results</h3>
                {map_tolerance_list}
                <p>
                    The following table shows which models were included in this week's verschilanalyse.
                    The table shows, for every model, whether or not it executed successfully
                    and whether any variable tolerance (his or map) was exceeded.
                    Moreover, the total computation time in seconds in both the current and the reference
                    verschilanalyse and the tolerance (1 percent) between the current and reference computation time
                    are displayed.
                </p>
                {table}
                <p>
                    Every verschilanalyse is run with a release of the D-Hydro software.
                    We can identify the release of the D-Hydro software with the
                    &quot;commit id&quot;. This identifies the snapshot of the
                    D-Hydro source code repository that was used to compile the
                    release.
                </p>
                <ul id="commit-id-list">
                    <li>Current verschilanalyse: {current_commit_id}</li>
                    <li>Reference verschilanalyse: {reference_commit_id}</li>
                </ul>
                <h3>Links</h3>
                {links_section}
            </body>
        </html>
        """
    ).strip()

    @staticmethod
    def _indent(s: str, level: int, spaces_per_level: int = 4) -> str:
        spaces = level * spaces_per_level
        return s.replace("\n", "\n" + spaces * " ")

    @staticmethod
    def _variable_exceeded(stats: Statistics, tol) -> bool:
        """Return True if any statistic exceeds tolerance."""
        return (
            stats.avg_max > tol.max or
            stats.avg_bias > tol.bias or
            stats.avg_rms > tol.rms
        )

    @classmethod
    def _variable_exceeded_in_output(
        cls,
        output: VerschillentoolOutput,
        var_name: str,
        variable_registry: VariableRegistry,
    ) -> bool:
        """Check whether a specific variable exceeded tolerance in a specific output."""
        stats = output.statistics[var_name]
        tol = variable_registry.get(var_name).tolerances[output.output_type]
        return cls._variable_exceeded(stats, tol)

    @classmethod
    def _any_variable_exceeded(
        cls,
        output: VerschillentoolOutput,
        variable_registry: VariableRegistry,
    ) -> bool:
        """Return True if any variable exceeds tolerance for the given output."""
        for var_name, stats in output.statistics.items():
            tol = variable_registry.get(var_name).tolerances[output.output_type]
            if cls._variable_exceeded(stats, tol):
                return True
        return False

    @classmethod
    def _exceeded_models(
        cls,
        model_outputs: dict[str, VerschillentoolOutput],
        variable_registry: VariableRegistry,
    ) -> Iterator[str]:
        """Yield model names where ANY variable exceeds tolerance."""
        for model_name, output in sorted(model_outputs.items()):
            if cls._any_variable_exceeded(output, variable_registry):
                yield model_name

    @classmethod
    def _to_rows(
        cls,
        comparisons: dict[str, LogComparison],
        his_outputs: dict[str, VerschillentoolOutput],
        map_outputs: dict[str, VerschillentoolOutput],
        variable_registry: VariableRegistry,
    ) -> Iterator[str]:

        # Compute exceeded sets ONCE (your proposed simplification)
        his_exceeded = set(cls._exceeded_models(his_outputs, variable_registry))
        map_exceeded = set(cls._exceeded_models(map_outputs, variable_registry))

        for model_name, comparison in sorted(comparisons.items()):
            current = comparison.current
            reference = comparison.reference

            crash = "❌ Crash" if current.is_crash() else "✅ Success"

            cur_comp_time = "❓"
            if current.mean_computation_time != 0.0:
                cur_comp_time = f"{current.mean_computation_time:.3f} s"

            ref_comp_time = "❓"
            comp_time_tolerance = "❓"
            if reference is not None and reference.mean_computation_time != 0.0:
                ref_comp_time = f"{reference.mean_computation_time:.3f} s"
                comp_time_diff = abs(current.mean_computation_time - reference.mean_computation_time)
                comp_time_percentage = (comp_time_diff / reference.mean_computation_time) * 100
                comp_time_tolerance = "❌ Exceeded" if comp_time_percentage > 1 else "✅ Success"

            # one tolerance column per variable
            tolerance_cells = []
            for var_name in variable_registry.all():
                exceeded = False

                # Check HIS
                if model_name in his_outputs:
                    if cls._variable_exceeded_in_output(his_outputs[model_name], var_name, variable_registry):
                        exceeded = True

                # Check MAP
                if not exceeded and model_name in map_outputs:
                    if cls._variable_exceeded_in_output(map_outputs[model_name], var_name, variable_registry):
                        exceeded = True

                tolerance_status = "❌ Exceeded" if exceeded else "✅ Success"
                tolerance_cells.append(f"<td>{tolerance_status}</td>")

            yield "".join(
                [
                    f"<td>{model_name}</td>",
                    f"<td>{crash}</td>",
                    *tolerance_cells,
                    f'<td class="align-right">{cur_comp_time}</td>',
                    f'<td class="align-right">{ref_comp_time}</td>',
                    f"<td>{comp_time_tolerance}</td>",
                ]
            )

    @classmethod
    def _format_model_run_table(
        cls,
        comparisons: dict[str, LogComparison],
        his_outputs: dict[str, VerschillentoolOutput],
        map_outputs: dict[str, VerschillentoolOutput],
        variable_registry: VariableRegistry,
    ) -> str:

        rows = "\n".join(
            f"<tr>{row}</tr>"
            for row in cls._to_rows(comparisons, his_outputs, map_outputs, variable_registry)
        )

        variable_headers = "".join(
            f"<th>{variable_registry.get(var_name).name} tolerances</th>"
            for var_name in variable_registry.all()
        )

        template = textwrap.dedent(
            """
            <table id="model-run-table">
                <tr>
                    <th>Model name</th>
                    <th>Execution status</th>
                    {variable_headers}
                    <th>Current computation time</th>
                    <th>Reference computation time</th>
                    <th>Computation time tolerance</th>
                </tr>
                {rows}
            </table>
            """
        ).strip()

        return template.format(
            rows=cls._indent(rows, 1),
            variable_headers=variable_headers,
        )

    @classmethod
    def _format_tolerance_list(
        cls,
        output_stats: dict[str, VerschillentoolOutput],
        output_type: str,
        variable_registry: VariableRegistry,
    ) -> str:

        exceeded_html = '<span style="color:red;">exceeded</span>'

        exceeded_models = list(cls._exceeded_models(output_stats, variable_registry))

        items = "\n".join(f"<li>{model}</li>" for model in exceeded_models)
        if not items:
            items = "<li>None: All variable differences are within tolerances.</li>"

        template = textwrap.dedent(
            f"""
            <p>Models where variable tolerances were {exceeded_html}:</p>
            <ul id="{output_type}-tolerance-list">
                {items}
            </ul>
            """
        ).strip()

        return template

    @classmethod
    def _format_model_list(cls, model_names: Iterable[str]) -> str:
        list_items = "\n".join(f"<li>{name}</li>" for name in sorted(model_names))
        if not list_items:
            list_items = "<li>None: No verschillentool output files were found.</li>"

        template = textwrap.dedent(
            """
            <ul id="model-list">
                {list_items}
            </ul>
            """
        ).strip()

        return template.format(list_items=cls._indent(list_items, 1))

    @classmethod
    def _format_links(cls, report_build_url: str, report_url: str) -> str:
        links = []
        if report_build_url:
            links.append(f'<li>{report_build_url}TeamCity report build.</li>')
        if report_url:
            for file_name, description in [
                ("current_logs.zip", "Download the logs for this verschilanalyse."),
                ("reference_logs.zip", "Download the logs for the reference verschilanalyse."),
                ("verschillen.zip", "Download the verschillentool output."),
            ]:
                links.append(f'<li>{report_url}/{file_name}</li>')

        link_lines = "\n".join(links)
        if not link_lines:
            link_lines = "<li>No links.</li>"

        template = textwrap.dedent(
            """
            <ul id="links">
                {link_lines}
            </ul>
            """
        ).strip()
        return template.format(link_lines=cls._indent(link_lines, 1))

    @staticmethod
    def _get_commit_ids(log_comparisons: dict[str, LogComparison]) -> tuple[str, str]:
        log_data_pairs = (
            (cmp.current, cmp.reference)
            for cmp in log_comparisons.values()
            if cmp.reference is not None
        )
        return next(
            (
                (cur.commit_id, ref.commit_id)
                for cur, ref in log_data_pairs
                if cur.commit_id and ref.commit_id
            ),
            ("", ""),
        )

    @classmethod
    def make_summary_page(
        cls,
        verschilanalyse: VerschilanalyseComparison,
        report_build_url: str,
        artifact_base_url: str,
        variable_registry: VariableRegistry,
    ) -> str:
        """Make an HTML formatted page containing information on this weeks verschilanalyse.

        This HTML page is used to format the weekly verschilanalyse email.
        It should contain enough information to see at a glance whether or
        not something went wrong or action needs to be taken.
        The `TEMPLATE` is used to format the email. Standard python formatting
        is used to fill in the template. A value needs to be provided for each
        of the bracketed expressions in the `TEMPLATE`.

        Parameters
        ----------
        verschilanalyse : VerschilanalyseComparison
            An object containing all of the information collected for
            the weekly automated verschilanalyse.
        report_build_url : str
            The URL of the "Report" build in the "Verschilanalyse" TeamCity project.
        artifact_base_url : str
            The base URL of the artifacts in the "Report" build.

        Returns
        -------
        str
            The HTML formatted content of the email.
        """
        log_comparisons = verschilanalyse.get_log_comparisons()
        current_commit_id, reference_commit_id = cls._get_commit_ids(log_comparisons)
        his_tolerance_list = cls._format_tolerance_list(verschilanalyse.his_outputs, OutputType.HIS.value, variable_registry)
        map_tolerance_list = cls._format_tolerance_list(verschilanalyse.map_outputs, OutputType.MAP.value, variable_registry)
        table = cls._format_model_run_table(log_comparisons, verschilanalyse.his_outputs, verschilanalyse.map_outputs, variable_registry)
        model_list = cls._format_model_list(verschilanalyse.his_outputs.keys())
        links_section = cls._format_links(report_build_url, artifact_base_url.rstrip("/"))

        result = cls.TEMPLATE.format(
            style=cls._indent(cls.STYLESHEET, 3),
            table=cls._indent(table, 2),
            current_commit_id=current_commit_id,
            reference_commit_id=reference_commit_id,
            current_prefix=f"{verschilanalyse.s3_current_prefix}/output",
            reference_prefix=f"{verschilanalyse.s3_reference_prefix}/output",
            model_list=cls._indent(model_list, 2),
            his_tolerance_list=cls._indent(his_tolerance_list, 2),
            map_tolerance_list=cls._indent(map_tolerance_list, 2),
            links_section=cls._indent(links_section, 2),
        )

        return result
