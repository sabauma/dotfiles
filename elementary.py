#!/usr/bin/env python
import cairo, sys, argparse
import math, random, copy
import os
import time

def random_rules():
 """Select a random rule each time"""
 return [int(random.getrandbits(1)) for _ in range(8)]

gruvbox = [
  (204, 36 , 29 ),
  (152, 151, 26 ),
  (215, 153, 33 ),
  ( 69, 133, 136),
  (177,  98, 134),
  (168, 153, 132),
  (254, 128,  25),
  (104, 157, 106)
]

backgrounds = (29, 32, 33)

def draw_background(cr, r, g, b, width, height):
    cr.set_source_rgb(r, g, b)
    cr.rectangle(0,0,width,height)
    cr.fill()

def draw_circle_fill(cr, x, y, radius, r, g, b):
    cr.set_source_rgb(r, g, b)
    cr.arc(x, y, radius, 0, 2*math.pi)
    cr.fill()

def rule_number(arr):
    return arr[2] * 4 + arr[1] * 2 + arr[0]

def generate(directory):
    width, height = 3840, 2160
    number_beziers = 480
    circle_size = 3
    x_d = width / number_beziers
    y_d = x_d

    load_factor = random.uniform(0.01, 0.5)
    random.shuffle(gruvbox)

    ims = cairo.ImageSurface(cairo.FORMAT_ARGB32, width, height)
    cr  = cairo.Context(ims)

    draw_background(cr, backgrounds[0]/255.0, backgrounds[1]/255.0, backgrounds[2]/255.0, width, height)

    rules = random_rules()
    seen = set()

    # Seed the starting row
    current_row = [int(random.uniform(0.0, 1.0) < load_factor) for _ in xrange(number_beziers)]
    for k in xrange(y_d * 3, height, y_d):

        first = rule_number((current_row[-1], current_row[0], current_row[1]))
        last  = rule_number((current_row[-2], current_row[-1], current_row[0]))
        # Determine the next row state by comparing rules
        rules_used = [rule_number(current_row[j:j+3]) for j in xrange(0, len(current_row) - 2)]
        rules_used = [first] + rules_used + [last]

        current_row = tuple((rules[r] for r in rules_used))
        if current_row in seen:
            return False

        seen.add(current_row)

        # Iterate through and draw the circles
        for i in xrange(1, len(current_row)):
            if current_row[i]:
                c = gruvbox[rules_used[i]]
                # c = random.choice(gruvbox)
                draw_circle_fill(cr, i * x_d, k, circle_size, c[0]/255.0, c[1]/255.0, c[2]/255.0)

    fname = directory + os.path.sep + "wallpaper.png"
    ims.write_to_png(fname)
    os.system("feh --bg-fill %s" % fname)

    return True

def main():
    while True:
        # Loop until we succeed
        while not generate("/tmp"):
            pass
        time.sleep(60 * 15)

if __name__ == "__main__":
    main()
