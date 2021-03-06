# Nogomet-Project

<p align="center">
 <span class="hljs-tag"><img src=https://am4pap001files.storage.live.com/y4mvCAdG8JF8uoPY6SR_bb1mz69hwbnFojzSK8bfRFXB9UGUBkyo0wA5axdyiVxwU3-jA3-f32olw1mIYppwpWL9a11_oz5CFSdCwWfoxNL4QYf3vXuUjfUbGlH3ahFJ1cNLu6ajXPiIbdYN-wT0ZWqlY6YaIfgBiqFULhJdHduRM6ALjtdIZQor9F9B8peCFLc?width=847&height=793&cropmode=none />
</p>

The __Nogomet__ application allows to visualize all the shots made according to a selection of filters. You can choose the desired comptition, the season, the desired team(s) and the type of shots.

All the shots will be represented on the soccer field on the right of the filtering area. The size variation depends directly on the expected goals (also knows as xG). The xG represent the probability of scoring the goal. You can choose the xG score you want to see on the soccer field with the xG slicer. To illustrate how an xG works let's take two simple examples:
  - the attacker scores with a 25 meter shot in the middle of a forest of opposing defenders. The xG will be very low (close to 0.022 so a probability of scoring of 2.2%) because it is a long shot, with defenders in front of him and a goalkeeper on his line.
  - the attacker scores while he is in the penalty area with an empty goal. Then the xG will be very high (close to 0.96 so a probability of scoring of 96%) because the goal is simple, close to him and without opponents in front of him.

The small soccer field just below the larger one allows you to select with your own cursor a place on the soccer field to directly filter where the balls can come from. 

The __Nogomet__ application also allows you to directly download the scorers table (defined according to the set of filters) in .csv format using the "download the data" button.


# Description of all files


* app. R gathers all the code necessary to realize the application (cleaning, ui part and server part)

* soccerfield is the R code file that allows to draw the soccer field

* the data folder contains all the data necessary for the creation of this project: 

  * the events folder contains all the events that took place during a soccer season

  * lineups contains all the information about the players

  * matches contains all the information about the matches that took place (date, home team, ...)

  * three-sixty includes all the information about the coordinates of the shots

* the pictures file contains all the images used to design the application


*The creation of this application is part of a university Big Data project (University of Tours). This application, being a university project, is not yet fully completed and has some bugs and areas for improvement.*

