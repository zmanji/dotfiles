#!/usr/bin/python3

from pathlib import Path
import os
from pprint import pprint

from http.server import HTTPServer, SimpleHTTPRequestHandler

from socket import socket


def launch_activate_socket(name: str):
    from ctypes import byref, CDLL, c_int, c_size_t, POINTER
    libc = CDLL('/usr/lib/libc.dylib')
    fds = POINTER(c_int)()
    count = c_size_t()

    res = libc.launch_activate_socket(name.encode('utf-8'), byref(fds), byref(count))
    if res:
        raise Exception(os.strerror(res))

    sockets = [socket(fileno=fds[s]) for s in range(count.value)]
    libc.free(fds)
    return sockets


def main():
    pprint(dict(os.environ))
    if not os.environ['XPC_SERVICE_NAME']:
        raise "Does not appear to be launched by launchd"
    # Name must match key in Sockets dict of plist
    sockets =  launch_activate_socket('Listeners')
    pprint(sockets)
    if len(sockets) != 1:
        raise Exception(f"Too many sockets: {len(sockets)}")
    socket = sockets[0]


    HTTPServer.address_family = socket.family
    SimpleHTTPRequestHandler.protocol_version = "HTTP/1.1"
    pprint(socket.getsockname())

    with HTTPServer(socket.getsockname(), SimpleHTTPRequestHandler, bind_and_activate=False) as httpd:
        httpd.socket = socket
        httpd.server_activate()

        host, port = httpd.socket.getsockname()[:2]
        url_host = f'[{host}]' if ':' in host else host
        print(
            f"Serving HTTP on {host} port {port} "
            f"(http://{url_host}:{port}/) ..."
        )
        print("About to server forever")
        httpd.serve_forever()
        #httpd.handle_request()
        print("Stopped serving forever")



if __name__ == '__main__':
    main()
