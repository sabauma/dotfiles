import cairo, sys, argparse
import math, random, copy

def random_rules():
 """Select a random rule each time"""
 rules = {
    (1, 1, 1) : int(random.getrandbits(1)),
    (1, 1, 0) : int(random.getrandbits(1)),
    (1, 0, 1) : int(random.getrandbits(1)),
    (1, 0, 0) : int(random.getrandbits(1)),
    (0, 1, 1) : int(random.getrandbits(1)),
    (0, 1, 0) : int(random.getrandbits(1)),
    (0, 0, 1) : int(random.getrandbits(1)),
    (0, 0, 0) : int(random.getrandbits(1))
 }
 return rules


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

backgrounds = (40, 40, 40)

def draw_background(cr, r, g, b, width, height):
    cr.set_source_rgb(r, g, b)
    cr.rectangle(0,0,width,height)
    cr.fill()

def draw_circle_fill(cr, x, y, radius, r, g, b):
    cr.set_source_rgb(r, g, b)
    cr.arc(x, y, radius, 0, 2*math.pi)
    cr.fill()

def index(arr):
    return arr[0] * 4 + arr[1] * 2 + arr[2]

def main():
    width, height = 3840, 2160
    number_beziers = 256
    circle_size = 5
    x_d = width / number_beziers
    y_d = x_d

    random.shuffle(gruvbox)

    ims = cairo.ImageSurface(cairo.FORMAT_ARGB32, width, height)
    cr  = cairo.Context(ims)

    draw_background(cr, backgrounds[0]/255.0, backgrounds[1]/255.0, backgrounds[2]/255.0, width, height)

    rules = random_rules()

    # Seed the starting row
    current_row = [int(random.getrandbits(1)) for _ in range(number_beziers)]
    next_row = current_row[:]
    colors = next_row[:]
    for k in xrange(y_d, height, y_d):
        # Determine the next row state by comparing rules
        for j in xrange(len(current_row) - 2):
            colors[j+1] = index(current_row[j:j+3])
            next_row[j+1] = rules[(current_row[j], current_row[j+1], current_row[j+2])]

        # Boundary conditions
        colors[0] = index((current_row[-1], current_row[0], current_row[1]))
        next_row[0]  = rules[(current_row[-1], current_row[0], current_row[1])]
        colors[-1] = index((current_row[-2], current_row[-1], current_row[0]))
        next_row[-1] = rules[(current_row[-2], current_row[-1], current_row[0])]

        # Iterate through and draw the circles
        for i in xrange(1, len(next_row)):
            if next_row[i]:
                c = random.choice(gruvbox)
                c = gruvbox[colors[i]]
                draw_circle_fill(cr, i * x_d, k, circle_size, c[0]/255.0, c[1]/255.0, c[2]/255.0)
        current_row, next_row = next_row, current_row
    ims.write_to_png('base.png')

if __name__ == "__main__":
    main()
