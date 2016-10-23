#!/usr/bin/env python

import subprocess
import re
import sys

def fail(message):
    sys.stderr.write(message + "\n")
    sys.exit(1)

class NoPulseError(Exception):
    pass
    
def run(*args):
    p = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    out, err = p.communicate()
    if p.returncode != 0:
        cmd = " ".join(args)
        errmsg = err.decode("utf-8")
        if "No PulseAudio daemon running" in errmsg:
            raise NoPulseError()
        msg = "error running external command: " + cmd + "\n\n" + errmsg
        fail(msg)
    return out.decode("utf-8")


change_amount = int(65536 / 10)
sink_summary = None
default_sink = None
sink_count = 0
default_sink_summary_offset = 0

def maybe_read_summary():
    global sink_summary
    global default_sink
    global sink_count
    global default_sink_summary_offset
    if default_sink is not None:
        return
    sink_summary = run("pacmd", "list-sinks")
    #print(sink_summary)
    index_re = re.compile(r"^\s*(\*?) index: (\d+)$", re.MULTILINE)
    offset = 0
    while True:
        m = index_re.search(sink_summary, offset)
        if not m:
            break
        if m.group(1) == '*':
            default_sink = int(m.group(2))
            default_sink_summary_offset = m.end(0)
        sink_count += 1
        offset = m.end(0)
    if default_sink is None:
        fail("no default sink found")

def get_volume():
    volume_re = re.compile(r"^\s*volume:\s+[a-z\-]+:\s+(\d+)\s+", re.MULTILINE)
    m = volume_re.search(sink_summary, default_sink_summary_offset)
    if not m:
        fail("could not find volume")
    return int(m.group(1))

def get_muted():
    muted_re = re.compile(r"^\s*muted:\s+(yes|no)\s*$", re.MULTILINE)
    m = muted_re.search(sink_summary, default_sink_summary_offset)
    if not m:
        fail("could not find muted status")
    return m.group(1) == "yes"

def clamp_volume(vol):
    if vol < 0:
        return 0
    if vol > 65535:
        return 65535
    return vol



def rotate_default():
    maybe_read_summary()
    run("pacmd", "set-default-sink", str((default_sink + 1) % sink_count))

def print_volume():
    try:
        maybe_read_summary()
    except NoPulseError:
        sys.stdout.write("<fc=#606060>pulse not running</fc>\n")
        return
    except:
        raise
    
    vol = get_volume()
    muted = get_muted()
    #print("vol: " + str(vol))
    #print("muted: " + str(muted))

    if muted:
        status = "muted"
    else:
        status = str(round(100.0 * vol / 65536.0)) + "%"

    result = "<fc=#ee9a00>{0}</fc> <fc=#909090>({1})</fc>".format(status, default_sink)
    sys.stdout.write(result)

def inc_volume():
    maybe_read_summary()
    vol = get_volume()
    vol = clamp_volume(vol + change_amount)
    run("pactl", "set-sink-volume", str(default_sink), str(vol))

def dec_volume():
    maybe_read_summary()
    vol = get_volume()
    vol = clamp_volume(vol - change_amount)
    run("pactl", "set-sink-volume", str(default_sink), str(vol))

def toggle_mute():
    maybe_read_summary()
    run("pactl", "set-sink-mute", str(default_sink), "toggle")




def main():
    if len(sys.argv) < 2:
        fail("must have arg: rotate_default|print_volume|inc_volume|dec_volume|toggle_mute")
    command = sys.argv[1]
    if command == "rotate_default":
        rotate_default()
    elif command == "print_volume":
        print_volume()
    elif command == "inc_volume":
        inc_volume()
    elif command == "dec_volume":
        dec_volume()
    elif command == "toggle_mute":
        toggle_mute()
    else:
        fail("invalid command: " + command)

if __name__ == "__main__":
    main()

