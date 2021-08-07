; definiting entities and state variables

breed [males male]
breed [females female]


patches-own [noise_level]

males-own [
  mated ; whether males mated either FALSE or the who number of the female
  total_noise ; total noise exposed before mating
  avg_noise ; average noise exposed before mating
  time_mating ; time when they mate
]

females-own [mated] ; whether are mated either FALSE or the who number of the male


; ------ core routines ----------

to setup
  clear-all
  setup-birds
  setup-patches
  reset-ticks
end


to go
  tick ; increment ticks at the beginning of the procedure to avoid a 0 tick and hence a division by zero in the average noise procedure

  ; Check if need to stop because enough time elapsed
  ; 14 is the number of days that the simulation is run
  ; 10 is the number of steps in a day
  if ticks > 14 * 10 [ ; stops afer the number of ticks
    stop
  ]
  ; females
  song_attraction
  females_move
  ; mating
  females_mate
  ; males
  males_move
  update_male_avg_noise
end



; ------------ SETUP --------------------

; setup the patches background noise according to the noise_distribution and the background_noise_level
to setup-patches

  ; ------
  ; This is needed only for the random distribution
  ; The patches groups have the same noise value, so there is need to generate the random numbers before entering the ask patches block.
  let patch_group_size 10 ; number of patches in a side of a group
  let num_cols world-width / patch_group_size
  let num_rows world-height / patch_group_size
  let n_patch_groups num_cols * num_rows ; total number of groups
  ; create a list with the values of background noise for each group from a uniform random distribution. The list lenght should be the number of groups
  let patch_group_noise map [x -> random-float background_noise_level] range n_patch_groups
  ; -------

  ask patches [

    ; all the patches have the same amount of noise
    if noise_distribution = "uniform" [
      set noise_level background_noise_level
    ]

    ; there is a gradient from the left side of the world to the right, going from noise 0 to background_noise_level
    if noise_distribution = "gradient" [
      set noise_level pxcor / world-width * background_noise_level ; pxcor / world-width gives a fraction on how far on the right the patch is, then is scaled using the background_noise_level
    ]

    ; The world is divided "vertically" in two sections one with 0 background noise and the other there is a background_noise_level
    if noise_distribution = "two-areas" [
      set noise_level (round (pxcor / world-width)) * background_noise_level ; round (pxcor / world-width) will return 0 for the first half and 1 for the second half. In this way there are two distinct areas
    ]

    ; random distribution: each group patch has its own value of background noise ranging from 0 to background_noise_level
    if noise_distribution = "random" [
      ; using the patch coordinates we need to get the number of the row and the number of the col in a virtual grid
      let n_col floor (pxcor /  patch_group_size)
      let n_row num_rows - floor (pycor /  patch_group_size) - 1; in netlogo the x axis is positive going up, invert it so the top left corner has a n_row equal to 0

      let group_index n_col + n_row * num_rows ; Need to get a unique index based on the position in the grid.
      ; Gets from the list of random noises the right value based on the position of the patch in the groups grid
      set noise_level item group_index patch_group_noise
    ]

    ; For visualization purposes sets the color of the patch on green scale color, when the noise is 0 the patch would be black, when is 1 white and for all the values in between of different shades of green.
    set pcolor scale-color green noise_level 0 1
  ]


end

; creates females and males birds and initalize their position
to setup-birds

  create-males n_males [

    ; by default random distribution for males
    setxy random-pxcor random-pycor

    set mated FALSE ; at the beginning no male is mated
    set  total_noise 0; init tracking variables to 0
    set avg_noise 0 ;
    set time_mating 0 ;

    set color orange - 1
    set shape "bird side"

    pen-down ; to keep track of movement
  ]

  ; if distribution is regular put them on a grid
  if male_distribution = "regular" [
    space_males_evenly
  ]

  ; create females
  create-females n_females [
    setxy random-pxcor random-pycor ; random distribution is the only option

    set mated FALSE ; at the beginning no female is mated

    set color cyan - 1
    set shape "bird side"

    pen-down
  ]
end

; put the males into a regular grid
to space_males_evenly
  ; calc the number of columns to have a evely spaced males in a square
  ; uses celiging to make sure that there is enough space for them
  ; in the case the number of males is not a perfect square there will be some empty spots in the grid
  let num_cols ceiling sqrt count males
  let num_rows num_cols ; same number cols are rows

  let horizontal_spacing (world-width / num_cols)
  let vertical_spacing (world-height / num_rows)

  ; add some padding on left and right
  let min_xpos (min-pxcor - 0.5 + horizontal_spacing / 2)
  let min_ypos (min-pycor - 0.5 + vertical_spacing / 2)

  ask males [
     let row (floor (who / num_cols)) ; uses the who number as a way to tell the different males apart
     let col (who mod num_cols)
     pen-up ; raise pen so it doesn't show the positioning on the grid
     setxy (min_xpos + col * horizontal_spacing)
           (min_ypos + row * vertical_spacing)
    pen-down
   ]
end

; ------------ Females --------------------

to song_attraction
  ask females with [mated = FALSE] [ ; only not mated females
    ; get the closest male in certain radius that is singing (not mated)
    let closest_male min-one-of males
          with [mated = FALSE]
          ; max radius where can hear male song
          in-radius song_radius
          ; sort by distance from female
          [distance myself]

    let here_noise_level [noise_level] of patch-here ; get current path noise level

    ; there is a probability 1 - noise level that the female will hear the male and go in that direction
    ; if so goes into the direction of the male
    ; Need to check that there is at least one male otherwise the face command has no meaning.
    ifelse random-float 1 < ( 1 - here_noise_level) and (closest_male != nobody)
    [
       face closest_male
    ]
    ; else go into a random direction
    [
      right (random 180) - 90 ; random direction between -90 and 90 degrees
    ]
  ]

end

; moves the females forward
to females_move
  ask females with [mated = FALSE] ; only not mated females
  [
    forward step_length
  ]
end

; Mating procedure for females
to females_mate
   ask females with [mated = FALSE] [

    ; get the closest not mated male
    let closest_male min-one-of males
          with [mated = FALSE]
          ; sort by distance from female
          [distance myself]

      if closest_male != nobody [ ; needed to check that there is a male otherwise next condition will fail at runtime
        ; if male is close enough can mate
        if distance closest_male < step_length [

          ; the female decides to mate with a prob_mating
          ifelse random-float 1 < prob_mating [
            mate self closest_male
          ]
          ; else decides that the male is not interesting and moves away
          ; move for more than the song radius so it doesn't get attracted again at the next step
          [
            ; goes into opposite direction of male
            face closest_male
            right 180
            ; moves away far enough to not hear the song anymore
            forward song_radius + 2 * step_length
          ]
        ]
      ]

  ]
end


; mates together the given male and female
to mate [female_bird male_bird]
  ask  male_bird
  [
     set mated female_bird ; set the mated status

     set color violet - 1 ; set to mated colour
     set time_mating ticks ; keep track of time of mating, only for males
   ]
  ask female_bird[
     move-to male_bird ; move exactly to the male so they fully overlap. Only for visualization purposes

     set mated male_bird ; set the mated status

     set color violet - 1 ; set to mated colour
  ]
end


; ------------ Males --------------------

; moves the male
to males_move
  ask males with [mated = FALSE] [ ; only to not mated males
    ; this allows males to move a bit but they won't change their position too much (random direction 0-360)
    right random 360 ; change to random direction in range 0-360
    forward step_length
  ]
end

; update the total and avg noise
to update_male_avg_noise
  ask males with [mated = FALSE] ; The average noise needs to be updated only for males who are not mated yet
  [
    ; to accurately calculate the average need to keep track of the total noise and then at each time update the avg_noise by dividing by the number of ticks
    set total_noise total_noise + [noise_level] of patch-here ; noise of current patch
    set avg_noise total_noise / ticks
  ]
end



; ------------ Plotting ----------------

; Utility functions needed for plotting

; get the avg_noise for mated or not mated. Simple reporter to make next code easier
to-report get_males_avg_noise [mated?]
  ifelse mated?
  [
    report [avg_noise] of males with [mated != FALSE]
  ]
  [
    report [avg_noise] of males with [mated = FALSE]
  ]
end

; This reporter is needed to handle the edge case when the are no mated males and the mean should be 0 instead of producing an error
; depending on the value of mated returns either the average of either mated or not mated males
to-report global_avg_noise [mated?]
  let males_avg_noise get_males_avg_noise mated?  ; getting the avg_noise of all mated males
  ifelse not empty? males_avg_noise [
    report mean males_avg_noise ; mean doesn't work on empty lists
  ]
  [
    report 0 ; default value in case there are no mated males
  ]
end

; return the maximum number of couples possible which is the minimum number of either males or females
to-report max_n_couples
  report min (list n_males n_females)
end
@#$#@#$#@
GRAPHICS-WINDOW
265
11
923
670
-1
-1
13.0
1
10
1
1
1
0
1
1
1
0
49
0
49
1
1
1
ticks
30.0

BUTTON
37
30
224
65
setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
36
81
159
114
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
166
82
221
115
step
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
38
158
228
191
n_males
n_males
0
100
100.0
1
1
NIL
HORIZONTAL

SLIDER
37
252
228
285
background_noise_level
background_noise_level
0
1
0.8
.1
1
NIL
HORIZONTAL

PLOT
939
142
1332
385
Ratio actual couples / max numer of couples
time
ratio actual couples
0.0
1.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count males with [mated != FALSE] / max_n_couples"

SLIDER
36
356
226
389
step_length
step_length
0
.7
0.7
.05
1
km
HORIZONTAL

SLIDER
37
403
226
436
song_radius
song_radius
.2
2
1.0
.2
1
km
HORIZONTAL

SLIDER
37
304
228
337
prob_mating
prob_mating
0
1
1.0
.1
1
NIL
HORIZONTAL

MONITOR
940
78
1329
123
Ratio actual couples / max numer of couples
count males with [mated != FALSE] / max_n_couples
2
1
11

MONITOR
939
15
1327
60
Number of couples
count males with [mated != FALSE]
0
1
11

CHOOSER
37
451
229
496
male_distribution
male_distribution
"random" "regular"
1

CHOOSER
37
516
228
561
noise_distribution
noise_distribution
"gradient" "uniform" "two-areas" "random"
0

PLOT
1366
419
1707
664
Distribution of males avg noise
Males average noise
Number of males
0.0
10.0
0.0
10.0
false
true
"set-plot-x-range 0 1.3 ;noise range\nset-plot-y-range 0 max_n_couples\nset-histogram-num-bars 4" ""
PENS
"Mated" 0.5 1 -10141563 true "" "histogram get_males_avg_noise TRUE"
"Not mated" 1.0 1 -3844592 true "set-histogram-num-bars 4" "histogram get_males_avg_noise FALSE"

PLOT
940
417
1324
662
Males average noise before mating
time
Average noise
0.0
6.0
0.0
1.0
true
true
"" ""
PENS
"Mated" 1.0 0 -10141563 true "" "plot global_avg_noise TRUE"
"Not mated" 1.0 0 -817084 true "" "plot global_avg_noise FALSE"

PLOT
1360
141
1708
384
Distribution of average mating time
time
Occurences of mating time
0.0
10.0
0.0
10.0
true
false
"set-plot-x-range 0 140 ; total number of ticks\nset-histogram-num-bars 10" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [time_mating] of males with [time_mating != 0]"

MONITOR
1364
79
1706
124
Averge mating time
mean [time_mating] of males with [time_mating != 0]
2
1
11

SLIDER
38
203
229
236
n_females
n_females
0
100
100.0
1
1
NIL
HORIZONTAL

TEXTBOX
270
694
602
799
Legend patch background:\n - black is 0 noise\n - white is noise 1\n - shades of green indicates the intermediate values
12
0.0
1

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

bird side
false
0
Polygon -7500403 true true 0 120 45 90 75 90 105 120 150 120 240 135 285 120 285 135 300 150 240 150 195 165 255 195 210 195 150 210 90 195 60 180 45 135
Circle -16777216 true false 38 98 14

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="First model sensitivity analysis" repetitions="20" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count males with [mated != FALSE] / count males</metric>
    <steppedValueSet variable="background_noise_level" first="0" step="0.2" last="1"/>
    <enumeratedValueSet variable="n_ticks">
      <value value="50"/>
      <value value="100"/>
      <value value="200"/>
    </enumeratedValueSet>
    <steppedValueSet variable="song_radius" first="1" step="1" last="6"/>
    <enumeratedValueSet variable="step_length">
      <value value="0.5"/>
      <value value="1"/>
      <value value="2"/>
    </enumeratedValueSet>
    <steppedValueSet variable="n_birds" first="10" step="10" last="50"/>
  </experiment>
  <experiment name="step lenght no attraction" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count males with [mated != FALSE] / count males</metric>
    <enumeratedValueSet variable="background_noise_level">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_ticks">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="song_radius">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="step_length">
      <value value="0.5"/>
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_birds">
      <value value="50"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="change noise level" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count males with [mated != FALSE] / count males</metric>
    <steppedValueSet variable="background_noise_level" first="0" step="0.1" last="1"/>
    <enumeratedValueSet variable="n_ticks">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="song_radius">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="step_length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n_birds">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob_mating">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
