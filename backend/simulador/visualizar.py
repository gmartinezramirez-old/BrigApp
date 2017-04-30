import pygame
from pygame import gfxdraw
from pygame.locals import *
from json import loads
from time import sleep

WIDTH = 640
HEIGHT = 480

var = 0
ls = loads(open("salida.json").read())




pygame.init()
screen = pygame.display.set_mode((WIDTH, HEIGHT))
pygame.display.set_caption("Pruebas Pygame")

def siguiente():
    global var
    global ls
    try:
        nex = ls[var]
    except:
        print "Fin"
        print len(ls)
        return
    for x in nex:
        if x["state"] >= 1:
            gfxdraw.pixel(screen, x["x"], x["y"], (255, 0, 0))
        else:
            gfxdraw.pixel(screen, x["x"], x["y"], (0, 255, 0))
    var = var + 1


while True:
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            pygame.quit()
            quit()

    print "Iteracion"
    siguiente()
    pygame.display.update()
    sleep(1)
