#! /usr/bin/env python3

def main():
    from wsgiref.simple_server import make_server
    from chokma.server import chokma_app
    print("Starting server...")
    httpd = make_server('localhost', 8080, chokma_app)
    httpd.serve_forever()

if __name__ == '__main__':
    import os
    os.environ['CHOKMA_CONFIG'] = 'test.config'
    from reload import run_with_reloader
    run_with_reloader(main)
    print("\nGoodbye!")

