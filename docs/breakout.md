[home](index.md)
[download](https://github.com/cmosher01/Apple-II-Source/releases/latest)
breakout

---

*Breakout*, an arcade video game, was designed by
**Steve Wozniak** beginning in October of 1975, for
Atari. His original design was ultimately rejected
by Atari, but Wozniak was inspired to design the
first *Apple computers* by this game. Many of the
features of the Apple \]\[ series computers were
designed with Breakout in mind. For example, the
paddles, the lo-res graphics screen, colors, sound,
and even the text area at the bottom of
the graphics screen.

This version of Breakout is written for
the APPLE \]\[ computer, in Integer BASIC.

This is the earliest version I have been able
to find so far. This one is from an original
version of the Apple \]\[ Reference Manual, entitled
*APPLE II MINI MANUAL* from some time in 1977.

### Original Version

Below is the original version, which can be pasted into an emulator (while
running Integer BASIC, of course).

``` visualbasic
  5 TEXT: CALL -936: VTAB 4: TAB 10: PRINT "*** BREAKOUT GAME ***": PRINT
  7 PRINT "  OBJECT IS TO DESTROY ALL BRICKS WITH 5 BALLS": FOR N=1 TO 7000: NEXT N
 10 DIM A$(20),B$(20): GR: PRINT: INPUT "HI, WHAT'S YOUR NAME? ",A$:A=1:B=13:C=9:D=6:E=15: PRINT "STANDARD COLORS, "; A$;
 20 INPUT "? ",B$: IF B$#"N" AND B$#"NO" THEN 30: FOR I=0 TO 39: COLOR=I/2*(I<32): VLIN 0,39 AT I
 25 NEXT I: POKE 34,20: PRINT: PRINT: PRINT: FOR I=0 TO 15: VTAB 21+I MOD 2: TAB I+I+1: PRINT I;: NEXT I: POKE 34,22: VTAB 24: PRINT: PRINT "BACKGROUND";
 27 GOSUB 100: A=E: PRINT "EVEN BRICK";: GOSUB 100: B=E: PRINT "ODD BRICK";: GOSUB 100: C=E: PRINT "PADDLE";: GOSUB 100: D=E : PRINT "BALL";: GOSUB 100
 30 POKE34,20:COLOR=A:FORI=0TO39:VLIN0,39ATI:NEXTI:FOR I=20TO34STEP2:TAB I+1:PRINT I/2-9;:COLOR=B:VLIN 0,39 AT I:COLOR=C:FOR J=I MOD 4 TO 39 STEP4
 35 VLIN J,J+1 AT I: NEXT J,I: TAB 5: PRINT "SCORE = 0": PRINT: PRINT: POKE 34,21: S=0: P=S: L=S: X=19: Y=19: X=19
 40 COLOR=A:PLOTX,Y/3:X=19:Y=RND(120):V=-1:W=RND(5)-2:L=L+1:IFL>5THEN140:TAB6:PRINT"BALL #";L:PRINT:FORI=1TO100:GOSUB200:NEXTI:M=1:N=0
 50 J=Y+W: IF J>=0 AND J<120 THEN 60: W=-W: J=Y: FOR I=1 TO 6: K=PEEK(-16336): NEXT I
 55 IF PEEK(-16287)>127 THEN SW=1-SW
 60 I=X+V: IF I<0 THEN 400: GOSUB 200: COLOR=A: K=J/3: IF I>39 THEN 70: IF SCRN(I,K)=A THEN 90: IF I THEN 120: N=N+1: V=(N>9)+1: W=(K-P)*2-5:M=1
 65 Z = PEEK(-16336)-PEEK(-16336)-PEEK(-16336)-PEEK(-16336)-PEEK(-16336)-PEEK(-16336)-PEEK(-16336): GOTO 90
 70 FOR I=1 TO 6: M=PEEK(-16336): NEXT I: I=X: M=0
 80 V=-V
 90 PLOT X,Y/3: COLOR=E: PLOT I,K: X=I: Y=J: GOTO 50
 99 PRINT "INVALID.  REENTER";
100 INPUT " COLOR (0 TO 15)",E: IF E<0 OR E>15 THEN 99: RETURN
120 IF M THEN V=ABS(V): VLIN K/2*2,K/2*2+1 AT I: S=S+I/2-9: VTAB 21: TAB 13: PRINT S
123 Q = PEEK(-16336)-PEEK(-16336)-PEEK(-16336)-PEEK(-16336)-PEEK(-16336)-PEEK(-16336)-PEEK(-16336)-PEEK(-16336)-PEEK(-16336)-PEEK(-16336)
124 IF S<720 THEN 80
130 PRINT "CONGRATULATIONS, YOU WIN.": GOTO 150
140 PRINT "YOUR SCORE OF ";S;" IS ";: GOTO 141+S/100
141 PRINT "TERRIBLE!": GOTO 150
142 PRINT "LOUSY.": GOTO 150
143 PRINT "POOR.": GOTO 150
144 PRINT "FAIR.": GOTO 150
145 PRINT "GOOD.": GOTO 150
146 PRINT "VERY GOOD.": GOTO 150
147 PRINT "EXCELLENT.": GOTO 150
148 PRINT "NEARLY PERFECT."
150 PRINT "SAME COLORS";: GOTO 20
200 IF SW THEN 220: Q=(PDL(0)-5)/6: IF Q<0 THEN Q=0
205 IF Q>=34 THEN Q=34: COLOR=D: VLIN Q,Q+5 AT 0: COLOR=A: IF P>Q THEN 210: IF Q THEN VLIN 0,Q-1 AT 0: P=Q: RETURN
210 IF P=Q THEN RETURN: IF Q#34 THEN VLIN Q+6,39 AT 0: P=Q: RETURN
220 Q=(Y-5)/3+RND(3)*SGN(W)*(X<10 AND V<0): IF Q<0 THEN Q=0: GOTO 205
400 FOR I=1 TO 80: Q=PEEK(-16336): NEXT I: GOTO 40
```

### Bugs and Anomalies

1. At line 205, "IF P>Q" should be "IF P>=Q".

1. At line 35, there is also one redundant piece of
   code: "X=19" appears twice on that line.

1. At line 35, also, the initialization of X and Y
   to 19 here is anomalous; it will cause the PLOT on
   line 40 to unnecessarily plot the background color
   at pixel (19,6). These initializations could instead
   simply be 0, which would be equally harmless.

1. At line 200, there is also an unnecessary check for
   Q<0. The calculation of Q in the previous statement
   will always leave Q between 0 and 41 inclusive.

1. There is a logic error in that the user can choose
   the same color for the background as one of the other
   objects, which can cause some strange game-play.
   *  If the even or odd brick color is chosen the same
      as the background color, the game cannot be won.
      For example, if the user chooses 1 (red) for the
      background and the even bricks (and chooses something
      else for the other colors) the game will start with
      only half the bricks appearing (because the half that
      are drawn red blend in with the background). You will
      never be able to hit these bricks, because the program
      checks the color (on line 60) "IF SCRN(I,K) = A" to see
      if you hit a brick. So you could clear all the visible
      bricks and have a score of only 360, half what you need
      to win the game. So the game will never end.
      You can get to this point right at the beginning of
      the game if BOTH bricks are chosen the same as
      the background color (your score will forever be 0).
   *  If the paddle is chosen the same as the background
      color, then the paddle will never hit the ball (even
      in demo. mode).
   *  If the ball is chosen the same as the background color,
      then you cannot see it, so it will be hard to hit it
      (although demo. mode works... if you can turn it on).
   Other object combinations of the same color are OK, such
   as even and odd bricks the same color, or the paddle the
   same color as the ball.

1. The program will crash with an error message of "\*** >32767 ERR"
   if the user enters a number between -32753 and -32767, inclusive,
   when entering a value for a custom color number. This is
   due to the IF statement on line 100.
   "IF E < 0 OR E > 15 THEN 99" should be
   "IF E < 0 THEN 99 : IF E > 15 THEN 99"

1. Entering more than 20 characters as a response to any question
   causes the program to abort with the error message
   "\*** STR OVFL ERR" (string overflow).

### Bugfix version

A version with fixes for bugs 1-6 described above.

``` visualbasic
  5 TEXT: CALL -936: VTAB 4: TAB 10: PRINT "*** BREAKOUT GAME ***": PRINT
  7 PRINT "  OBJECT IS TO DESTROY ALL BRICKS WITH 5 BALLS": FOR N=1 TO 7000: NEXT N
 10 DIM A$(20),B$(20): GR: PRINT: INPUT "HI, WHAT'S YOUR NAME? ",A$:A=1:B=13:C=9:D=6:E=15: PRINT "STANDARD COLORS, "; A$;
 20 INPUT "? ",B$: IF B$#"N" AND B$#"NO" THEN 30: FOR I=0 TO 39: COLOR=I/2*(I<32): VLIN 0,39 AT I
 25 NEXT I: POKE 34,20: PRINT: PRINT: PRINT: FOR I=0 TO 15: VTAB 21+I MOD 2: TAB I+I+1: PRINT I;: NEXT I: POKE 34,22: VTAB 24: PRINT: PRINT "BACKGROUND";
 27 A=16:GOSUB 100: A=E: PRINT "EVEN BRICK";: GOSUB 100: B=E: PRINT "ODD BRICK";: GOSUB 100: C=E: PRINT "PADDLE";: GOSUB 100: D=E : PRINT "BALL";: GOSUB 100
 30 POKE34,20:COLOR=A:FORI=0TO39:VLIN0,39ATI:NEXTI:FOR I=20TO34STEP2:TAB I+1:PRINT I/2-9;:COLOR=B:VLIN 0,39 AT I:COLOR=C:FOR J=I MOD 4 TO 39 STEP4
 35 VLIN J,J+1 AT I: NEXT J,I: TAB 5: PRINT "SCORE = 0": PRINT: PRINT: POKE 34,21: S=0: P=0: L=0: X=0: Y=0
 40 COLOR=A:PLOTX,Y/3:X=19:Y=RND(120):V=-1:W=RND(5)-2:L=L+1:IFL>5THEN140:TAB6:PRINT"BALL #";L:PRINT:FORI=1TO100:GOSUB200:NEXTI:M=1:N=0
 50 J=Y+W: IF J>=0 AND J<120 THEN 60: W=-W: J=Y: FOR I=1 TO 6: K=PEEK(-16336): NEXT I
 55 IF PEEK(-16287)>127 THEN SW=1-SW
 60 I=X+V: IF I<0 THEN 400: GOSUB 200: COLOR=A: K=J/3: IF I>39 THEN 70: IF SCRN(I,K)=A THEN 90: IF I THEN 120: N=N+1: V=(N>9)+1: W=(K-P)*2-5:M=1
 65 Z = PEEK(-16336)-PEEK(-16336)-PEEK(-16336)-PEEK(-16336)-PEEK(-16336)-PEEK(-16336)-PEEK(-16336): GOTO 90
 70 FOR I=1 TO 6: M=PEEK(-16336): NEXT I: I=X: M=0
 80 V=-V
 90 PLOT X,Y/3: COLOR=E: PLOT I,K: X=I: Y=J: GOTO 50
 99 PRINT "INVALID.  REENTER";
100 INPUT " COLOR (0 TO 15)",E: IF E<0 THEN 99 : IF E>15 THEN 99: IF E=A THEN 99 : RETURN
120 IF M THEN V=ABS(V): VLIN K/2*2,K/2*2+1 AT I: S=S+I/2-9: VTAB 21: TAB 13: PRINT S
123 Q = PEEK(-16336)-PEEK(-16336)-PEEK(-16336)-PEEK(-16336)-PEEK(-16336)-PEEK(-16336)-PEEK(-16336)-PEEK(-16336)-PEEK(-16336)-PEEK(-16336)
124 IF S<720 THEN 80
130 PRINT "CONGRATULATIONS, YOU WIN.": GOTO 150
140 PRINT "YOUR SCORE OF ";S;" IS ";: GOTO 141+S/100
141 PRINT "TERRIBLE!": GOTO 150
142 PRINT "LOUSY.": GOTO 150
143 PRINT "POOR.": GOTO 150
144 PRINT "FAIR.": GOTO 150
145 PRINT "GOOD.": GOTO 150
146 PRINT "VERY GOOD.": GOTO 150
147 PRINT "EXCELLENT.": GOTO 150
148 PRINT "NEARLY PERFECT."
150 PRINT "SAME COLORS";: GOTO 20
200 IF SW THEN 220: Q=(PDL(0)-5)/6
205 IF Q>=34 THEN Q=34: COLOR=D: VLIN Q,Q+5 AT 0: COLOR=A: IF P>=Q THEN 210: IF Q THEN VLIN 0,Q-1 AT 0: P=Q: RETURN
210 IF P=Q THEN RETURN: IF Q#34 THEN VLIN Q+6,39 AT 0: P=Q: RETURN
220 Q=(Y-5)/3+RND(3)*SGN(W)*(X<10 AND V<0): IF Q<0 THEN Q=0: GOTO 205
400 FOR I=1 TO 80: Q=PEEK(-16336): NEXT I: GOTO 40
```

### Variables

* `A$`:: name of user
* `B$`:: temporary user input value
* `A`::  color of background
* `B`::  color of even bricks
* `C`::  color of odd bricks
* `D`::  color of paddle
* `E`::  color of ball
* `I`::  X coord of next ball pos
* `J`::  Y coord of next ball pos * 3
* `K`::  Y coord of next ball pos (`J/3`, plottable Y coord)
* `L`::  current ball number (1-5)
* `M`::  indicates if we have not just hit the back wall (is usually 1, but is 0 when coming off the back wall until next hit)
* `N`::  count of paddle hits for the current ball
* `P`::  paddle position (Y coord of top pixel of paddle)
* `Q`::  temporary value
* `S`::  score
* `SW`:: are we in demo. mode? (1 = yes, auto paddle movement; 0 = no, user moves paddle)
* `V`::  ball X movement (`V<0` means left, `V>0` means right, `ABS(V)` is speed)
* `W`::  ball Y movement (`W<0` means up,   `W>0` means down,  `ABS(W)` is speed/angle)
* `X`::  ball X position (0-39) (0 is at paddle, 39 is at back wall)
* `Y`::  ball Y position * 3 (0-119) (0 is top of screen, 119 is bottom of screen)

### Implementation Notes

To make the ball "bounce," change the sign of one of the coordinates
of movement. For example, when the ball is approaching the paddle,
V (the X movement) is negative; to make the ball bounce away toward
the right, set V = -V, which makes V positive, thus making the ball bounce.

Note that all calculations on the Y coordinate are done internally at
a three times scale. Then only upon drawing is the Y coordinate divided
by three (and rounded down). This allows for a precision of one third pixel
(even though we are not using floating point numbers).

Bricks in the first column are 1 point; bricks in the second column are
2 points; etc. up to 8 points per brick in the eighth column. Each brick
is two pixels tall, so there are 20 bricks in each column. That makes
a total of 720 points for all the bricks.

While playing the game, if the paddle button is depressed while the ball
is bouncing off the top or bottom of the screen, it will toggle "demo"
mode. If demo mode is on, then the game controller is ignored, and instead
the program automatically moves the paddle on the screen to hit the ball.


---


## Line-by-Line Annotated Version



### Initialization



#### line 5

set "text" mode (turn off any graphics display)
``` visualbasic
TEXT :
```

clear the screen
``` visualbasic
CALL -936 :
```

display title
``` visualbasic
VTAB 4 : TAB 10 : PRINT "*** BREAKOUT GAME ***" : PRINT
```



#### line 7

display instructions
``` visualbasic
PRINT "  OBJECT IS TO DESTROY ALL BRICKS WITH 5 BALLS" :
```

wait a few seconds
``` visualbasic
FOR N = 1 TO 7000 : NEXT N
```



#### line 10

allocate string variables ("dimension")
``` visualbasic
DIM
```

username (20 characters maximum)
``` visualbasic
A$(20) ,
```

temporary user input string (20 characters maximum)
``` visualbasic
B$(20) :
```

set low resolution graphics mode (40x40 pixels)
``` visualbasic
GR :
```

get user's name
``` visualbasic
PRINT : INPUT "HI, WHAT'S YOUR NAME? ",A$ :
```

set the default colors

background (red)
``` visualbasic
A=1  :
```

even brick (yellow)
``` visualbasic
B=13 :
```

odd brick (orange)
``` visualbasic
C=9  :
```

paddle (blue)
``` visualbasic
D=6  :
```

ball (white)
``` visualbasic
E=15 :
```

ask the user if she wants to use the standard color scheme
``` visualbasic
PRINT "STANDARD COLORS, "; A$;
```



### Color Customization



#### line 20

if the user does want the default colors,
then skip the next section (go to line 30)
``` visualbasic
INPUT "? ", B$ : IF B$#"N" AND B$#"NO" THEN 30 :
```

display color bars and ask the user to choose a
color for each of the displayed elements of the game.

draw color bars
``` visualbasic
FOR I = 0 TO 39 :
    COLOR = I/2*(I<32) :
    VLIN 0, 39 AT I
```




#### line 25

``` visualbasic
NEXT I :
```

set four-line window at bottom of screen:
``` visualbasic
POKE 34, 20 : PRINT : PRINT : PRINT :
```

display color numbers
``` visualbasic
FOR I = 0 TO 15 :
    VTAB 21+I MOD 2 :
    TAB I+I+1 :
    PRINT I; :
NEXT I :
```

set two-line window at bottom of screen:
``` visualbasic
POKE 34, 22 :
```

ask user each color, and put into vars A-E
``` visualbasic
VTAB 24 : PRINT :

PRINT "BACKGROUND";
```


#### line 27
subroutine at line 100
``` visualbasic
                      GOSUB 100 : A=E :
PRINT "EVEN BRICK"; : GOSUB 100 : B=E :
PRINT  "ODD BRICK"; : GOSUB 100 : C=E :
PRINT     "PADDLE"; : GOSUB 100 : D=E :
PRINT       "BALL"; : GOSUB 100
```



### Game Initialization



#### line 30

new game starts here (after optionally picking colors)

set 4-line text window at bottom
``` visualbasic
POKE 34, 20 :
```

draw background
``` visualbasic
COLOR = A : FOR I = 0 TO 39 : VLIN 0, 39 AT I : NEXT I :
```

draw bricks and column numbers (points)
``` visualbasic
FOR I = 20 TO 34 STEP 2 :
    TAB I+1 : PRINT I/2-9; :
    COLOR = B : VLIN 0, 39 AT I :
    COLOR = C :
    FOR J = I MOD 4 TO 39 STEP 4
```



#### line 35
``` visualbasic
      VLIN J, J+1 AT I :
    NEXT J,
I :
```

display score
``` visualbasic
TAB 5 : PRINT "SCORE = 0" : PRINT : PRINT :
```

set 3-line window at bottom
``` visualbasic
POKE 34, 21 :
```

initialize score (col n brick is n points; 720 is full game all bricks)
``` visualbasic
S = 0 :
```

initialize paddle position (Y coord of top pixel of paddle)
``` visualbasic
P = S :
```

initialize current ball number
``` visualbasic
L = S :
```

Initialize ball X position.
Note that X must start out less than 20 so it does not
erase a brick in the PLOT at line 40.
``` visualbasic
X = 19 :
```

initialize ball Y position * 3
``` visualbasic
Y = 19 :
```

BUG: redundant initialization of X
``` visualbasic
X = 19
```



### Ball Movement and Collision



#### line 40

New ball starts here.



Erase any previous ball
``` visualbasic
COLOR = A : PLOT X, Y/3 :
```

Set initial ball postion (X,Y) and movement vectors (V,W)


Set X coordinate to be at the bricks, and Y coordinate
as a random 0-39 position (times 3).
``` visualbasic
X = 19 : Y = RND(120) :
```

Initialize ball movement.

X movement:: V<0 means left, V>0 means right, ABS(V) is speed
Y movement:: W<0 means up, W>0 means down, ABS(W) is speed/angle

``` visualbasic
V = -1 : W = RND(5)-2 :
```

next ball
``` visualbasic
L = L+1 :
```

if we just missed our last ball, then the game is over (go to line 140)
``` visualbasic
IF L > 5 THEN 140 :
```

display ball number
``` visualbasic
TAB 6 : PRINT "BALL #"; L : PRINT :
```

do not start the game yet, wait a while (but still let the user
move the paddle via subroutine at line 200)

``` visualbasic
FOR I = 1 TO 100 : GOSUB 200 : NEXT I :
```

we did not just bounce off the back wall
``` visualbasic
M = 1 :
```

count of paddle hits for the current ball
``` visualbasic
N = 0
```



#### line 50

Calculate next Y coordinate of ball (new Y = old Y + Ymovement)
``` visualbasic
J = Y+W :
```

If new value did not go off top or bottom, go to line 60
``` visualbasic
IF J >= 0 AND J < 120 THEN 60 :
```

else, the ball hit the top or bottom, so we need to make it bounce off

set new Ymovement (if was moving up, then set to down; if was moving down, then set to up)
``` visualbasic
W = -W :
```

restore Y coordinate (this keeps the ball at the top or bottom edge
of the screen, instead of off-screen; it will bounce on the next iteration)
``` visualbasic
J = Y :
```

make a sound to indicate bounce
``` visualbasic
FOR I = 1 TO 6 : K = PEEK(-16336) : NEXT I
```



#### line 55

if paddle button depressed, toggle demonstration mode on or off
``` visualbasic
IF PEEK(-16287) > 127 THEN SW = 1-SW
```



#### line 60

Calculate next X coordinate of ball (new X = old X + Xmovement)
``` visualbasic
I = X+V :
```

if we went off the left edge, the user missed the ball, so go to line 400
``` visualbasic
IF I < 0 THEN 400 :
```

else, the ball is still in play somewhere

move the displayed paddle based on user control
via soubroutine at line 200
``` visualbasic
GOSUB 200 :
```

set to background color (to prepare for erasing a hit brick)
``` visualbasic
COLOR = A :
```

calc plottable Y coordinate (into K)
``` visualbasic
K = J/3 :
```

if we hit the back wall (behind the bricks) go to line 70
``` visualbasic
IF I > 39 THEN 70 :
```

check what is on the screen at the position of the ball,
in order to find out what it hit (if anything)

if we did not hit anything (background color), then go to line 90
``` visualbasic
IF SCRN(I,K) = A THEN 90 :
```

else, we must have hit either a brick or the paddle;
we can tell which one by checking the X coordinate of the ball.

if X > 0, then we hit a brick, so go to line 120
``` visualbasic
IF I THEN 120 :
```

else, we must have hit the paddle, so
increment count of paddle hits for this ball
``` visualbasic
N = N+1 :
```

set Xmovement to positive (towards the bricks), and at a speed of 1 (slow)
if we have had less than 10 hits this ball, or a speed of 2 (fast) if we
have had 10 or more hits.
``` visualbasic
V = (N>9)+1 :
```

set Ymovement

The position of the ball on the paddle is K-P, which will be from 0 to 5,
0 being the top of the paddle, and 5 being the bottom. The sets of possible
values of the terms in the calculation of W, below, is shown here:
```
 K-P = { 0,  1,  2,  3,  4,  5 }
  *2 = { 0,  2,  4,  6,  8, 10 }
  -5 = {-5, -3, -1,  1,  3,  5 }
```
These represent the angle (up or down) that the ball bounces
off the paddle. So striking near the middle of the paddle sends the
ball straighter, and striking near the edge of the paddle sends the
ball at a more oblique angle, up (for positive W) or down (for negative W)
``` visualbasic
W = (K-P)*2-5 :
```

indicate that we did not just bounce off the back wall
``` visualbasic
M = 1
```


#### line 65

make a sound, and go to line 90
``` visualbasic
Z = PEEK(-16336)-PEEK(-16336)-PEEK(-16336)-PEEK(-16336)-PEEK(-16336)-PEEK(-16336)-PEEK(-16336) :
GOTO 90
```



#### line 70
(we come here if the ball has hit the back wall)

make a sound
``` visualbasic
FOR I = 1 TO 6 :
  M = PEEK(-16336) :
NEXT I :
```

restore Xpos (keep ball at back wall, instead of off the screen)
``` visualbasic
I = X :
```

indicate that we have hit the back wall
``` visualbasic
M = 0
```



#### line 80
ball bounces off of back wall or a brick, so toggle X direction
``` visualbasic
V = -V
```



#### line 90

erase previous ball position (plot background color)
``` visualbasic
PLOT X, Y/3 :
```

display the ball (new ball position I,K)
``` visualbasic
COLOR = E :
PLOT I, K :
```

store new ball position (I,J) into current ball position (X,Y)
``` visualbasic
X = I :
Y = J :
```

Go to line 50
``` visualbasic
GOTO 50
```



### Color Input Subroutine



#### line 99

function to ask user for a color (enter at line 100).

output:
[horizontal]
E:: the color that the user chose (0-15)

``` visualbasic
PRINT "INVALID.  REENTER";
```


#### line 100
Ask user to enter color. If OK, then return from function,
otherwise go to line 99 to print error message.
``` visualbasic
INPUT " COLOR (0 TO 15)", E : IF E < 0 OR E > 15 THEN 99 :

RETURN
```



### Hit a Brick



#### line 120

We have hit a brick.

We need to fix the ball direction.
We do this by making sure the sign of V is correct.
Negative V means move left, and positive V means move right.
However, we will later be toggling the sign (at line 80),
so here we set it to the opposite of what we need it to be.
Usually, when we hit a brick we want to move left after that,
so set V positive here (and it will become negative at line 80).
However there is a special case; after we hit the back wall,
we want the ball to be able to bounce off the back side of the
bricks and go towards the back wall again. So if we have just
hit the back wall (indicated by M being zero), then leave
V as negative (and it will switch to positive at line 80, so
the ball will move right, towards the back wall again).

``` visualbasic
IF M THEN V = ABS(V) :
```

erase the whole brick (each brick is two pixels tall)
``` visualbasic
VLIN K/2*2, K/2*2+1 AT I :
```

increase score
``` visualbasic
S = S+I/2-9 :
```

display score
``` visualbasic
VTAB 21 : TAB 13 :
PRINT S
```



#### line 123

make a sound
``` visualbasic
Q = PEEK(-16336)-PEEK(-16336)-PEEK(-16336)-PEEK(-16336)-PEEK(-16336)-
    PEEK(-16336)-PEEK(-16336)-PEEK(-16336)-PEEK(-16336)-PEEK(-16336)
```

#### line 124

if user has not cleared all the bricks yet, then continue playing (go to line 80)
``` visualbasic
IF S < 720 THEN 80
```
else the user won, so fall through:



### Game Over



#### line 130

game over (a win enters here; a loss enters at line 140)

print the evaluation message corresponding to the user's score,
then go to line 150

a score of 720 is a win
``` visualbasic
PRINT "CONGRATULATIONS, YOU WIN." : GOTO 150
```

#### line 140

go to appropriate line number depending on score (in hundreds)
``` visualbasic
PRINT "YOUR SCORE OF "; S; " IS "; : GOTO 141+S/100
```

#### line 141
score 0-99
``` visualbasic
PRINT "TERRIBLE!" :
GOTO 150
```

#### line 142
score 100-199
``` visualbasic
PRINT "LOUSY." :
GOTO 150
```

#### line 143
score 200-299
``` visualbasic
PRINT "POOR." :
GOTO 150
```

#### line 144
score 300-399
``` visualbasic
PRINT "FAIR." :
GOTO 150
```

#### line 145
score 400-499
``` visualbasic
PRINT "GOOD." :
GOTO 150
```

#### line 146
score 500-599
``` visualbasic
PRINT "VERY GOOD." :
GOTO 150
```

#### line 147
score 600-699
``` visualbasic
PRINT "EXCELLENT." :
GOTO 150
```

#### line 148
score 700-719
``` visualbasic
PRINT "NEARLY PERFECT."
```



#### line 150

go back to let user pick colors and restart the game, to line 20

``` visualbasic
PRINT "SAME COLORS"; :
GOTO 20
```



### Paddle Movement Subroutine



#### line 200

function to move displayed paddle based on user control

in/out:
[horizontal]
P:: on input, previous position; on output, current position (where position is Y coord of top-most pixel of paddle, 0-34)

in:
[horizontal]
A:: background color
D:: paddle color
SW:: demo. mode (1 or 0)

if in demo. mode, then also input:
[horizontal]
V:: ball X movement
W:: ball Y movement
X:: ball X position
Y:: ball Y position

if in demo. mode, then go to line 220
``` visualbasic
IF SW THEN 220 :
```

get paddle postion (Q) (top end of paddle)

PDL(0) paddle 0 -> 0 to 255

-5  ->  -5 to 250

/6  ->  0 to 41

``` visualbasic
Q = (PDL(0)-5)/6 :
```

BUG: unnecessary check for +Q < 0+; acutally Q cannot be less than zero here
``` visualbasic
IF Q < 0 THEN Q = 0
```

#### line 205

constrain Q [0,34]
``` visualbasic
IF Q >= 34 THEN Q = 34 :
```

draw paddle 6 pixels tall, possible positions: (0,5) to (34,39)
``` visualbasic
COLOR = D :
VLIN Q, Q+5 AT 0 :
```

erase old paddle

BUG: +P > Q+ should be +P >= Q+
``` visualbasic
COLOR = A :
IF P > Q THEN 210 :
```

paddle moved down, so erase area above paddle, and return
``` visualbasic
IF Q THEN VLIN 0, Q-1 AT 0 :
P = Q :
RETURN
```

#### line 210

paddle did not move, so just return
``` visualbasic
IF P = Q THEN RETURN :
```
paddle moved up, so erase area below paddle, and return
``` visualbasic
IF Q # 34 THEN VLIN Q+6, 39 AT 0 :
P = Q :
RETURN
```




#### line 220

demo. mode: ignore real paddle, instead automatically position the
paddle so it hits the ball

if the ball is coming towards the paddle (V<0), and
is close to the paddle (X<10), then add some random wiggle to the paddle
``` visualbasic
Q = (Y-5)/3  +  RND(3)*SGN(W)*(X<10 AND V<0) :
IF Q < 0 THEN Q = 0 :
```

we only come here from line 200, so we always return to
line 205
``` visualbasic
GOTO 205
```



### Sound for Missed the Ball



#### line 400

make a sound, then go to line 40
``` visualbasic
FOR I = 1 TO 80 : Q = PEEK(-16336) : NEXT I :
GOTO 40
```
