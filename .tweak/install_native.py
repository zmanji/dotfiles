#!/usr/local/bin/python3

import os
import pathlib
import json


def main():
    name = "dottweak"
    description = "dottweak native messenger"
    path = str(pathlib.Path(__file__).parent.absolute() / "native.py")
    base = {"type": "stdio", "name": name, "path": path, "description": description}

    firefox = base.copy()
    firefox["allowed_extensions"] = ["dottweak@zameermanji.com"]

    chrome = base.copy()
    chrome["allowed_origins"] = ["chrome-extension://phgjajoacbmgcaaaknciegifjiacjfim/"]

    path = "~/Library/Application Support/Mozilla/NativeMessagingHosts/dottweak.json"
    with open(os.path.expanduser(path), "w") as f:
        json.dump(firefox, f, indent=4)

    path = (
        "~/Library/Application Support/Google/Chrome/NativeMessagingHosts/dottweak.json"
    )
    with open(os.path.expanduser(path), "w") as f:
        json.dump(chrome, f, indent=4)


if __name__ == "__main__":
    main()
