import textwrap
from typing import ClassVar, Iterable, Iterator

from ci_tools.verschilanalyse.util.excel_exporter import LogComparison
from ci_tools.verschilanalyse.util.verschilanalyse_comparison import VerschilanalyseComparison
from ci_tools.verschilanalyse.util.verschillentool import OutputType, Tolerances, Variable, VerschillentoolOutput


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
                    and whether a water level tolerance (his or map) or flow velocity tolerance (his or map) was
                    exceeded. Moreover, the total computation time in seconds in both the current and the reference
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

    @classmethod
    def _to_rows(
        cls,
        comparisons: dict[str, LogComparison],
        his_outputs: dict[str, VerschillentoolOutput],
        map_outputs: dict[str, VerschillentoolOutput],
    ) -> Iterator[str]:
        water_lvl_exceeded = set(cls._exceeded_water_level_models(his_outputs)) | set(
            cls._exceeded_water_level_models(map_outputs)
        )
        flow_vel_exceeded = set(cls._exceeded_flow_velocity_models(his_outputs)) | set(
            cls._exceeded_flow_velocity_models(map_outputs)
        )

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
                comp_time_tolerance = "✅ Success"
                if comp_time_percentage > 1:
                    comp_time_tolerance = "❌ Exceeded"

            water_tolerance = "✅ Success"
            if model_name in water_lvl_exceeded:
                water_tolerance = "❌ Exceeded"

            flow_tolerance = "✅ Success"
            if model_name in flow_vel_exceeded:
                flow_tolerance = "❌ Exceeded"

            yield "".join(
                [
                    f"<td>{model_name}</td>",
                    f"<td>{crash}</td>",
                    f"<td>{water_tolerance}</td>",
                    f"<td>{flow_tolerance}</td>",
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
    ) -> str:
        rows = "\n".join(f"<tr>{row}</tr>" for row in cls._to_rows(comparisons, his_outputs, map_outputs))

        template = textwrap.dedent(
            """
            <table id="model-run-table">
                <tr>
                    <th>Model name</th>
                    <th>Execution status</th>
                    <th>Water level tolerances</th>
                    <th>Flow velocity tolerances</th>
                    <th>Current computation time</th>
                    <th>Reference computation time</th>
                    <th>Computation time tolerance</th>
                </tr>
                {rows}
            </table>
            """
        ).strip()

        return template.format(rows=cls._indent(rows, 1))

    @staticmethod
    def _exceeded_water_level_models(model_outputs: dict[str, VerschillentoolOutput]) -> Iterator[str]:
        for model_name, output in sorted(model_outputs.items()):
            water_lvl_stats = output.water_level
            if (
                water_lvl_stats.avg_max > Tolerances.max(output.output_type, Variable.WATER_LEVEL)
                or water_lvl_stats.avg_bias > Tolerances.bias(output.output_type, Variable.WATER_LEVEL)
                or water_lvl_stats.avg_rms > Tolerances.rms(output.output_type, Variable.WATER_LEVEL)
            ):
                yield model_name

    @staticmethod
    def _exceeded_flow_velocity_models(model_outputs: dict[str, VerschillentoolOutput]) -> Iterator[str]:
        for model_name, output in sorted(model_outputs.items()):
            flow_vel_stats = output.flow_velocity
            if (
                flow_vel_stats.avg_max > Tolerances.max(output.output_type, Variable.FLOW_VELOCITY)
                or flow_vel_stats.avg_rms > Tolerances.rms(output.output_type, Variable.FLOW_VELOCITY)
                or flow_vel_stats.avg_bias > Tolerances.bias(output.output_type, Variable.FLOW_VELOCITY)
            ):
                yield model_name

    @classmethod
    def _format_tolerance_list(cls, output_stats: dict[str, VerschillentoolOutput], output_type: str) -> str:
        exceeded_html = '<span style="color:red;">exceeded</span>'

        water_lvl_items = "\n".join(f"<li>{model}</li>" for model in cls._exceeded_water_level_models(output_stats))
        if not water_lvl_items:
            exceeded_html = "exceeded"
            water_lvl_items = "<li>None: All water level differences are within tolerances.</li>"

        flow_vel_items = "\n".join(f"<li>{model}</li>" for model in cls._exceeded_flow_velocity_models(output_stats))
        if not flow_vel_items:
            exceeded_html = "exceeded"
            flow_vel_items = "<li>None: All flow velocity differences are within tolerances.</li>"

        template = textwrap.dedent(
            f"""
            <p>Models where the water level tolerances were {exceeded_html}:</p>
            <ul id="{output_type}-water-level-tolerance-list">
                {water_lvl_items}
            </ul>
            <p>Models where the flow velocity tolerances were {exceeded_html}:</p>
            <ul id="{output_type}-flow-velocity-tolerance-list">
                {flow_vel_items}
            </ul>
            """
        ).strip()

        return template.format(
            output_type=output_type,
            water_lvl_items=cls._indent(water_lvl_items, 1),
            flow_vel_items=cls._indent(flow_vel_items, 1),
        )

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
            links.append(f'<li><a href="{report_build_url}">TeamCity report build.</a></li>')
        if report_url:
            for file_name, description in [
                ("current_logs.zip", "Download the logs for this verschilanalyse."),
                ("reference_logs.zip", "Download the logs for the reference verschilanalyse."),
                ("verschillen.zip", "Download the verschillentool output."),
            ]:
                links.append(f'<li><a href="{report_url}/{file_name}">{description}</a></li>')

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
        log_data_pairs = ((cmp.current, cmp.reference) for cmp in log_comparisons.values() if cmp.reference is not None)
        return next(
            ((cur.commit_id, ref.commit_id) for cur, ref in log_data_pairs if cur.commit_id and ref.commit_id),
            ("", ""),
        )

    @classmethod
    def make_summary_page(
        cls,
        verschilanalyse: VerschilanalyseComparison,
        report_build_url: str,
        artifact_base_url: str,
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
        his_tolerance_list = cls._format_tolerance_list(verschilanalyse.his_outputs, OutputType.HIS.value)
        map_tolerance_list = cls._format_tolerance_list(verschilanalyse.map_outputs, OutputType.MAP.value)
        table = cls._format_model_run_table(log_comparisons, verschilanalyse.his_outputs, verschilanalyse.map_outputs)
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
