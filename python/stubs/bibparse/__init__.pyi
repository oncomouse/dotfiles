from typing import Dict
from typing import List


def to_bibtex(key: str, value: List[str]) -> str: ...
def to_python(key: str, value: str) -> List[str]: ...


class Biblio(dict):
    def read(self, file: str) -> None: ...


class BibItem(dict):
    ...
