import re
import tkinter

WIDTH  = 600
HEIGHT = 100
g_points = None
g_root_win = None
g_canvas = None

def parse(x):
    matches = re.search(r"^position=<\s*([-\d]+),\s*([-\d]+)\s*>\s*velocity=<\s*([-\d]+),\s*([-\d]+)\s*>", x)
    if matches is not None and len(matches.groups()) == 4:
        return (int(matches.group(1)), int(matches.group(2)), int(matches.group(3)), int(matches.group(4)))
    return None

def get_example():
    with open("example.txt") as f:
        return [x for x in [parse(x) for x in f.read().split("\n")] if x is not None]

def get_input():
    with open("input.txt") as f:
        return [x for x in [parse(x) for x in f.read().split("\n")] if x is not None]

def offset(pt, dt):
    return (
        pt[0] + (pt[2] * dt),
        pt[1] + (pt[3] * dt),
    )

def render(time):
    global g_points, g_root_win, g_canvas, WIDTH, HEIGHT
    if g_points is None or g_canvas is None:
        return

    g_canvas.create_rectangle(0, 0, WIDTH + 6, HEIGHT + 6, fill="white")

    new_pts = [offset(x, time) for x in g_points]

    mins = (
        min(new_pts, key=lambda x: x[0])[0],
        min(new_pts, key=lambda x: x[1])[1],
    )
    maxs = (
        max(new_pts, key=lambda x: x[0])[0],
        max(new_pts, key=lambda x: x[1])[1],
    )

    scale_x = WIDTH  / abs(maxs[0] - mins[0])
    scale_y = HEIGHT / abs(maxs[1] - mins[1])

    for pt in new_pts:
        x = (pt[0] - mins[0]) * scale_x
        y = (pt[1] - mins[1]) * scale_y

        g_canvas.create_rectangle(x - 3, y - 3, x + 3, y + 3, fill="black")

def change_time(v):
    render(10100 + int(v) / 50)

def setup_window():
    root = tkinter.Tk()

    canvas = tkinter.Canvas(root, width=WIDTH + 6, height=HEIGHT + 6)
    canvas.pack(fill="both", side="top")

    scale = tkinter.Scale(
        root,
        orient=tkinter.HORIZONTAL,
        from_=0,
        to=100,
        command=change_time,
    )
    scale.set(50)
    scale.pack(fill="x", side="bottom")

    return (root, canvas)

def run():
    global g_points, g_root_win, g_canvas
    g_points = get_input()
    #g_points = get_example()
    (g_root_win, g_canvas) = setup_window()
    render(10101)
    g_root_win.mainloop()

if __name__ == "__main__":
    run()
