# CS 357 Final Project
## Contributors
Natalie Runyan

Daniel Thompson

Joel Villarreal

## Controls
'w' to jump

'd' to move right

'a' to move left

'e' to shoot projectiles (sheep magic)

space to swing sword

CONTROLS DO NOT WORK IF YOU HAVE CAPSLOCK ON

This is because the pattern matching in event handler does not include upper case w,a,d,e. 

## Usage
A sheep platform game! Shoot or stab enemies as you work your way from heaven to hell to retrieve your sinner sheep lover.  
The story line was supposed to be like a sort of dantes inferno, orpheus and eurydice type thing, but we ran out of time to really make that happen.

Special thanks to Haskell Curry. 

## Known Bugs
In the intro it says go west, when it really means go east. This is a refernce to the fact that I (Natalie) am dslexsic.

Collison is not exactly pixel perfect, but for the most part it should work in the users favor, enimies have a slightly bigger hit box and the players hit box is slightly smaller. If you want to see the hit boxes, in main there is commented out code, for every object that you just have to add into the existing list of pictures. It will not work if you just uncomment it.

There is like a 4 second period after the sheep gets injured by a projectile where you cannot get injured again, but theres no visual indication of that.

Still a bit of weirdness with vertical collison, if you hold down the movement keys sometimes you can push through blocks and fall of the map.

You can climb up a wall of blocks most of the time, and enimies can shoot you through blocks.

You are less likely to get injured if you are standing still, but sometimes it still happens, HOWEVER, if you'd like, you can think of this like the goat defense-mechanism of playing dead.

Theres not really an ending yet, or a fleshed out story line.


## Work Distribution
