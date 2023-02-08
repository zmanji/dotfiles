#!/usr/bin/env python3

import os
import json
import posixpath
import struct
import sys
import time
import traceback
import urllib.parse
from pathlib import Path


#LOG_FILE_PATH = "./" + time.strftime("%Y%m%d-%H%M%S") + ".log"
LOG_FILE_PATH = os.devnull
LOG_FILE = open(LOG_FILE_PATH, "w")

def log(s):
    print(s, file=LOG_FILE)
    LOG_FILE.flush()


def my_excepthook(type, value, tb):
    log("fatal exception")
    log(type)
    log(value)
    traceback.print_tb(tb, file=LOG_FILE)
    log("exiting ....")
    sys.exit(1)

sys.excepthook = my_excepthook

# Read a message from stdin and decode it.
def getMessage():
    rawLength = sys.stdin.buffer.read(4)
    if len(rawLength) == 0:
        log("terminating...")
        sys.exit(0)
    messageLength = struct.unpack('@I', rawLength)[0]
    message = sys.stdin.buffer.read(messageLength).decode('utf-8')
    return json.loads(message)

# Encode a message for transmission,
# given its content.
def encodeMessage(messageContent):
    encodedContent = json.dumps(messageContent).encode('utf-8')
    encodedLength = struct.pack('@I', len(encodedContent))
    return {'length': encodedLength, 'content': encodedContent}

# Send an encoded message to stdout
def sendMessage(encodedMessage):
    sys.stdout.buffer.write(encodedMessage['length'])
    sys.stdout.buffer.write(encodedMessage['content'])
    sys.stdout.buffer.flush()

# given url like www.example.com/hello/world
# return:
# www.example.com
# www.example.com/hello
# www.example.com/hello/world
def explodeUrl(url):
    ret = []


    url_parsed = urllib.parse.urlparse(url)

    # This seems to infinite loop in certain cases so abort early with not nor
    if url_parsed.scheme not in ("http", "https"):
        return ret

    base = url_parsed.netloc

    ret.append(base)

    path = posixpath.normpath(url_parsed.path)
    path_fragments = []
    while path != "/":
        ( path, item ) = posixpath.split(path)
        path_fragments.insert(0, item)

    for i in range(len(path_fragments)):
        ret.append(os.path.join(base, *path_fragments[:i+1]))

    return ret

# For each part get a .js and .css file from the fs
def getFiles(parts):
    base = os.path.join(Path.home(),".tweak")

    files = []

    for p in parts:
        files.append(os.path.normpath(os.path.join(base, p + ".js")))
        files.append(os.path.normpath(os.path.join(base, p + ".css")))

    log("all candidate files...")
    log(files)

    files = [Path(f) for f in files if Path(f).exists()]

    return files


# For each file, read and send
def sendFiles(files):
    log("about to send files ...")
    for f in files:
        log(f)
        log(f.suffix)
        msg = {}
        if f.suffix == ".css":
            msg["type"] = "CSS"
            msg["content"] = f.read_text()
        elif f.suffix == ".js":
            msg["type"] = "JS"
            msg["content"] = f.read_text()
        log(msg)
        sendMessage(encodeMessage(msg))

def main():
    log("booting")
    log(sys.argv)
    while True:
        receivedMessage = getMessage()
        log("recieved:")
        log(receivedMessage)
        if receivedMessage.get("url") != None:
            log("message has url ...")
            parts = explodeUrl(receivedMessage["url"])
            log("exploded url... see parts")
            log(parts)
            files = getFiles(parts)
            log("exploded url... see files")
            log(files)
            sendFiles(files)
            sendMessage(encodeMessage({"type": "DONE"}))

if __name__ == "__main__":
    main()
