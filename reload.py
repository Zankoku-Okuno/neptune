import sys, os, subprocess, time
import _thread as thread


def iter_module_files():
    """Return generator of filenames of loaded python modules."""
    # The list call is necessary in case the module
    # dictionary modifies during iteration.
    for module in list(sys.modules.values()):
        filename = getattr(module, '__file__', None)
        if filename:
            old = None
            while not os.path.isfile(filename):
                old = filename
                filename = os.path.dirname(filename)
                if filename == old:
                    break
            else:
                if filename[-4:] in ('.pyc', '.pyo'):
                    filename = filename[:-1]
                yield filename


def reloader_loop(extra_files=(), interval=1, restart_code=3):
    """When this function is run from the main thread, it will force other
threads to exit when any modules currently loaded change.

Copyright notice. This function is based on the autoreload.py from
the CherryPy trac which originated from WSGIKit which is now dead.

:param extra_files: a list of additional files it should watch.
"""
    from itertools import chain
    mtimes = {}
    while 1:
        for filename in chain(iter_module_files(), extra_files):
            try:
                mtime = os.stat(filename).st_mtime
            except OSError:
                continue

            old_time = mtimes.get(filename)
            if old_time is None:
                mtimes[filename] = mtime
                continue
            elif mtime > old_time:
                # _log('info', ' * Detected change in %r, reloading' % filename)
                sys.exit(restart_code)
        time.sleep(interval)



def restart_with_reloader(restart_code=3):
    """Spawn a new Python interpreter with the same arguments as this one,
but running the reloader thread.
"""
    while 1:
        # _log('info', ' * Restarting with reloader')
        args = [sys.executable] + sys.argv
        new_environ = os.environ.copy()
        new_environ['RELOADING'] = 'true'

        exit_code = subprocess.call(args, env=new_environ)
        if exit_code != restart_code:
            # print("Fix problem then press ENTER to continue...", file=sys.stderr)
            # sys.stdin.readline()
            return exit_code


def run_with_reloader(main_func, extra_files=(), interval=1, restart_code=3):
    """Run the given function in an independent python interpreter."""
    import signal
    signal.signal(signal.SIGTERM, lambda *args: sys.exit(0))
    if os.environ.get('RELOADING') == 'true':
        thread.start_new_thread(main_func, ())
        try:
            reloader_loop(extra_files, interval, restart_code)
        except KeyboardInterrupt:
            pass
    else:
        try:
            sys.exit(restart_with_reloader(restart_code))
        except KeyboardInterrupt:
            pass


