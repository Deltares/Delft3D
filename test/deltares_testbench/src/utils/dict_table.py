from typing import Dict, List

import numpy as np


class DictTable:
    def __init__(self, values: Dict[str, List[str]]) -> None:
        self.__values_dict = values
        self.__column_values = [r for r in self.__values_dict.values()]

    @property
    def headers(self) -> List[str]:
        return list(self.__values_dict.keys())

    @staticmethod
    def format_value(value) -> str:
        if isinstance(value, float) or type(value) in {
            np.float16,
            np.float32,
            np.float64,
        }:
            return f"{value:.3e}"
        else:
            return str(value)

    def max_column_width(self, key: str) -> int:
        default_width = len(key)
        return max(
            default_width, *[len(self.format_value(v)) for v in self.__values_dict[key]],
        )

    def row_values(self, row_index: int) -> List:
        return [cv[row_index] for cv in self.__column_values]

    def number_of_rows(self) -> int:
        first_key = next(enumerate(self.__values_dict.keys()))[1]
        return len(self.__values_dict[first_key])
