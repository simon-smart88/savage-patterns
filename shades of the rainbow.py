# -*- coding: utf-8 -*-
"""
Created on Tue Nov  4 00:02:01 2014

@author: simon
"""

svgo="<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" version=\"1.1\">"
svge="</svg>"

styopen="<style type=\"text/css\"><![CDATA["
styclose="]]></style>"

clas="<ellipse class=\""
clasend=">"

eopen="<ellipse class=\""
eopenend="\">"
eclose="</ellipse>"
estyle="    ellipse {stroke: black; stroke-width: 1; fill:black;	}"

cx="<animate attributeName=\"cx\" values=\""
cy="<animate attributeName=\"cy\" values=\""
rx="<animate attributeName=\"rx\" values=\""
ry="<animate attributeName=\"ry\" values=\""
sc=";"
dur="\" dur=\"20s\" repeatCount=\"indefinite\"/>" 

dot=" ."
animo=" { -webkit-animation: "
animc=" 20s infinite;}"
opo="@-webkit-keyframes "
opa=" { 0% { fill-opacity: "
opb=" ; } 50% { fill-opacity: "
opc=" ; } 100% { fill-opacity: "
ope=" ; }	}"

groupo="<g id=\""
tile="tile"
cent="cent"
groupc="\">"
groupe="</g>"

useo="<use xlink:href=\"#"
usetrans="\" transform=\"rotate("
usec=")\"/>"

fillo="<animate attributeName=\"fill\" values=\"#60C;#6CF;#060;#FF6;#F60;#C00;#F39;#60C\" repeatCount=\"indefinite\"  begin=\""
fille="s\" dur=\"50s\"/>"
minus="-"

def cya(n):
    cya2=(40*n)+100
    return cya2
    
def cyb(n):
    cyb2=((2*n**2)+(16*n)-12)+100
    return cyb2

def cyc(n):
    cyc2=(-(2*n**2)+(64*n)+12)+100
    return cyc2
    
def cyd(n):
    cyd2=-(2*n**2)+(112*n)-488
    return cyd2

def cye(n):
    cye2=(2*n**2)-(32*n)+688
    return cye2

def cxa(n):
    cxa2=(40*n)+100
    return cxa2
    
def cxb(n):
    cxb2=((2*n**2)+(16*n)-12)+100
    return cxb2

def cxc(n):
    cxc2=(-(2*n**2)+(64*n)+12)+100
    return cxc2

def cxd(n):
    cxd2=-(2*n**2)+(112*n)-488
    return cxd2

def cxe(n):
    cxe2=(2*n**2)-(32*n)+688
    return cxe2

def rya(n):
    rya2=20
    return rya2
    
def ryb(n):
    ryb2=8+(2*n)
    return ryb2

def ryc(n):
    ryc2=-(2*n)+32
    return ryc2
    
def ryd(n):
    ryd2=-(2*n)+56
    return ryd2    

def rye(n):
    rye2=(2*n)-16
    return rye2

def rxa(n):
    rxa2=20
    return rxa2
    
def rxb(n):
    rxb2=8+(2*n)
    return rxb2

def rxc(n):
    rxc2=-(2*n)+32
    return rxc2
    
def rxd(n):
    rxd2=-(2*n)+56
    return rxd2

def rxe(n):
    rxe2=(2*n)-16
    return rxe2

def opva(n):
    opva2=1-(n*0.03)
    return opva2

def opvb(n):
    opvb2=0.25+(n*0.03)
    return opvb2
    
def filla(n):
    filla2=0.5*n
    return filla2

"""open svg"""
print svgo

"""print css"""
print styopen

"""style for all ellipses"""
print estyle

"""style opacity animations"""
for i in range(26):
    print dot+(chr(i + ord('a')))+animo+(chr(i + ord('a')))+animc
    print opo+(chr(i + ord('a')))+opa+str(opva(i))+opb+str(opvb(i))+opc+str(opva(i))+ope

print styclose

"""print top left tile and center"""
print groupo+tile+groupc
for j in range(13): #x sets
    for i in range(13): #y sets
        print eopen+(chr(i+j + ord('a')))+eopenend
        print fillo+minus+str(filla(i+j))+fille
        print cx+str(cxa(j))+sc+str(cxb(j))+sc+str(cxa(j))+sc+str(cxc(j))+sc+str(cxa(j))+dur 
        print cy+str(cya(i))+sc+str(cyb(i))+sc+str(cya(i))+sc+str(cyc(i))+sc+str(cya(i))+dur  
        print rx+str(rxa(i))+sc+str(rxb(j))+sc+str(rxa(i))+sc+str(rxc(j))+sc+str(rxa(i))+dur 
        print ry+str(rya(i))+sc+str(ryb(i))+sc+str(rya(i))+sc+str(ryc(i))+sc+str(rya(i))+dur  
        print eclose
print groupe

"""print top right tile - use xd, xe, yb, yc"""
print groupo+cent+groupc
for j in range (13,25,1): #x = 13
    for i in range(13):
        print eopen+(chr(-j+24+i + ord('a')))+eopenend
        print fillo+minus+str(filla(-j+24+i))+fille
        print cx+str(cxa(j))+sc+str(cxd(j))+sc+str(cxa(j))+sc+str(cxe(j))+sc+str(cxa(j))+dur 
        print cy+str(cya(i))+sc+str(cyb(i))+sc+str(cya(i))+sc+str(cyc(i))+sc+str(cya(i))+dur  
        print rx+str(rxa(i))+sc+str(rxd(j))+sc+str(rxa(i))+sc+str(rxe(j))+sc+str(rxa(i))+dur 
        print ry+str(rya(i))+sc+str(ryb(i))+sc+str(rya(i))+sc+str(ryc(i))+sc+str(rya(i))+dur  
        print eclose
print groupe

"""print bottom left tile - use xb, xc, yd, ye"""
for j in range (13): #x = 13
    for i in range(13,25,1):
        print eopen+(chr(-i+24+j + ord('a')))+eopenend
        print fillo+minus+str(filla(-i+24+j))+fille
        print cx+str(cxa(j))+sc+str(cxb(j))+sc+str(cxa(j))+sc+str(cxc(j))+sc+str(cxa(j))+dur 
        print cy+str(cya(i))+sc+str(cyd(i))+sc+str(cya(i))+sc+str(cye(i))+sc+str(cya(i))+dur  
        print rx+str(rxa(i))+sc+str(rxb(j))+sc+str(rxa(i))+sc+str(rxc(j))+sc+str(rxa(i))+dur 
        print ry+str(rya(i))+sc+str(ryd(i))+sc+str(rya(i))+sc+str(rye(i))+sc+str(rya(i))+dur  
        print eclose

"""print bottom right tile - use xd, xe, yd, ye"""
for j in range (13,25,1): #x = 13
    for i in range(13,25,1):
        print eopen+(chr((-i+23+(-j+25)) + ord('a')))+eopenend
        print fillo+minus+str(filla(-i+23+(-j+25)))+fille
        print cx+str(cxa(j))+sc+str(cxd(j))+sc+str(cxa(j))+sc+str(cxe(j))+sc+str(cxa(j))+dur 
        print cy+str(cya(i))+sc+str(cyd(i))+sc+str(cya(i))+sc+str(cye(i))+sc+str(cya(i))+dur  
        print rx+str(rxa(i))+sc+str(rxd(j))+sc+str(rxa(i))+sc+str(rxe(j))+sc+str(rxa(i))+dur 
        print ry+str(rya(i))+sc+str(ryd(i))+sc+str(rya(i))+sc+str(rye(i))+sc+str(rya(i))+dur  
        print eclose

"""print tiles
print useo+tile+usetrans+str(90)+usec
print useo+cent+usetrans+str(180)+usec
"""
"""
http://sarasoueidan.com/blog/svg-transformations/
<use xlink:href="#tile" transform="rotate(90), translate(0,-1148)"/>
<use xlink:href="#tile" transform="rotate(180), translate(-1148,-1148)"/>
<use xlink:href="#tile" transform="rotate(270), translate(-1148,0)"/>
"""

print svge
