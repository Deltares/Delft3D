from typing import Dict

from ._zsf_cffi import ffi, lib


def _struct_to_dict(struct):
    d = {}

    for k in dir(struct):
        d[k] = getattr(struct, k)

        if not isinstance(d[k], (float, int, str)) and ffi.typeof(d[k]).cname.startswith("struct "):
            d[k] = _struct_to_dict(d[k])

    return d


def _zsf_error_message(code):
    return ffi.string(lib.zsf_error_msg(code)).decode("utf-8")


def _zsf_version():
    return ffi.string(lib.zsf_version()).decode("utf-8")


def zsf_calc_steady(auxiliary_results: bool = False, **parameters: float) -> Dict[str, float]:
    """
    Calculate the salt intrusion for a set of parameters, assuming steady
    operation.

    :param auxiliary_results: Whether or not to calculate and output auxiliary
        results. See :c:struct:`zsf_aux_results_t`.
    :param kwargs: Any parameters that should be changed versus the default.
        See also :c:struct:`zsf_param_t` for an overview of the parameters.

    :returns: A dictionary containing the cycle averaged salt fluxes and
        discharges (see :c:struct:`zsf_results_t`). Also outputs values in
        :c:struct:`zsf_aux_results_t` if ``auxiliary_results`` is `True`.
    """
    param_t = ffi.new("zsf_param_t *")

    # Check input parameters
    param_names = set(dir(param_t))
    for p in parameters:
        if p not in param_names:
            raise TypeError(f"No such parameter '{p}'")

    # Set default values
    lib.zsf_param_default(param_t)

    # Set parameter values based on keyword arguments
    for p, v in parameters.items():
        setattr(param_t, p, v)

    # Get results
    results_t = ffi.new("zsf_results_t *")
    if auxiliary_results:
        aux_results_t = ffi.new("zsf_aux_results_t *")
    else:
        aux_results_t = ffi.NULL
        assert len(dir(aux_results_t)) == 0

    err = lib.zsf_calc_steady(param_t, results_t, aux_results_t)

    if err:
        raise RuntimeError(_zsf_error_message(err))

    # Reformat results into a dictionary and return
    return {**_struct_to_dict(results_t), **_struct_to_dict(aux_results_t)}


class ZSFUnsteady:
    """
    A class to calculate a lock in phase-wise fashion.
    """

    def __init__(self, sal_lock, head_lock, **parameters: float):

        self._param_t = ffi.new("zsf_param_t *")
        self._state_t = ffi.new("zsf_phase_state_t *")
        # We can reuse the same object for results,
        # as we convert it to a dictionary before returning
        self._results_t = ffi.new("zsf_phase_transports_t *")

        self._param_t_names = set(dir(self._param_t))

        # Set default values
        lib.zsf_param_default(self._param_t)

        # Set user parameters
        self._set_parameters(**parameters)

        # Initialize the state
        lib.zsf_initialize_state(self._param_t, self._state_t, sal_lock, head_lock)

    def _set_parameters(self, **parameters: float):
        for p, v in parameters.items():
            if p not in self._param_t_names:
                raise TypeError(f"No such parameter '{p}'")
            else:
                setattr(self._param_t, p, v)

    def step_phase_1(self, t_level, **parameters: float) -> Dict[str, float]:
        """
        Level the lock to lake side. See also :c:func:`zsf_step_phase_1` .

        :param t_level: Duration of the leveling in seconds.
        :param parameters: Any parameters that should be changed before
            performing this step. Note that these changes persist.

        :returns: The salt and water transports in this phase.
                  See also :c:struct:`zsf_phase_transports_t`.
        """

        self._set_parameters(**parameters)

        err = lib.zsf_step_phase_1(self._param_t, t_level, self._state_t, self._results_t)
        if err:
            raise RuntimeError(_zsf_error_message(err))

        return _struct_to_dict(self._results_t)

    def step_phase_2(self, t_open_lake: float, **parameters: float) -> Dict[str, float]:
        """
        Open the door on lake side. See also :c:func:`zsf_step_phase_2` .

        :param t_open_lake: Duration the door is open in seconds.
        :param parameters: Any parameters that should be changed before
            performing this step. Note that these changes persist.

        :returns: The salt and water transports in this phase.
                  See also :c:struct:`zsf_phase_transports_t`.

        .. note: This function assumes that the lock is already at lake level before opening.
        """

        self._set_parameters(**parameters)

        err = lib.zsf_step_phase_2(self._param_t, t_open_lake, self._state_t, self._results_t)
        if err:
            raise RuntimeError(_zsf_error_message(err))

        return _struct_to_dict(self._results_t)

    def step_phase_3(self, t_level, **parameters: float) -> Dict[str, float]:
        """
        Level the lock to sea side. See also :c:func:`zsf_step_phase_3` .

        :param t_level: Duration of the leveling in seconds.
        :param parameters: Any parameters that should be changed before
            performing this step. Note that these changes persist.

        :returns: The salt and water transports in this phase.
                  See also :c:struct:`zsf_phase_transports_t`.
        """

        self._set_parameters(**parameters)

        err = lib.zsf_step_phase_3(self._param_t, t_level, self._state_t, self._results_t)
        if err:
            raise RuntimeError(_zsf_error_message(err))

        return _struct_to_dict(self._results_t)

    def step_phase_4(self, t_open_sea: float, **parameters: float) -> Dict[str, float]:
        """
        Open the door on sea side. See also :c:func:`zsf_step_phase_4` .

        :param t_open_sea: Duration the door is open in seconds.
        :param parameters: Any parameters that should be changed before
            performing this step. Note that these changes persist.

        :returns: The salt and water transports in this phase.
                  See also :c:struct:`zsf_phase_transports_t`.

        .. note: This function assumes that the lock is already at sea level before opening.
        """

        self._set_parameters(**parameters)

        err = lib.zsf_step_phase_4(self._param_t, t_open_sea, self._state_t, self._results_t)
        if err:
            raise RuntimeError(_zsf_error_message(err))

        return _struct_to_dict(self._results_t)

    def step_flush_doors_closed(self, t_flushing: float, **parameters: float) -> Dict[str, float]:
        """
        Open the door on sea side. See also :c:func:`zsf_step_flush_doors_closed` .

        :param t_flushing: Duration of flushing is open in seconds.
        :param parameters: Any parameters that should be changed before
            performing this step. Note that these changes persist.

        :returns: The salt and water transports in this phase.
                  See also :c:struct:`zsf_phase_transports_t`.

        .. note: This function makes no assumption on the lock level with respect to
                 the sea or lake levels.
        """

        self._set_parameters(**parameters)

        err = lib.zsf_step_flush_doors_closed(
            self._param_t, t_flushing, self._state_t, self._results_t
        )
        if err:
            raise RuntimeError(_zsf_error_message(err))

        return _struct_to_dict(self._results_t)

    @property
    def state(self) -> Dict[str, float]:
        """
        Get the state of the lock, see also :c:struct:`zsf_phase_state_t`.

        Note that this is a read-only property, and any changes made to the
        dictionary returned by this property do not persist.
        """

        return _struct_to_dict(self._state_t)
