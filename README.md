# AIPP_cookingbot
A Symbolic AI project based on Fluid Construction Grammar paradigm. Idea is to implement a cookingbot that primarily understands recipes and execute them. 

To run the AIPP cookingbot, follow the following instructions:

- Install the **latest version of the FCG Editor** from [https://www.fcg-net.org/download]()
- Clone this repository somewhere sensible
- Open `start.fcg` in the FCG Editor
- Adapt `*cookingbot-path*`
- Click `Evaluate file` in the toolbar
- Open `recipes/almond-cookies-recipe.fcg`
- Set the initial kitchen state and process the instructions 
- Open the web interface

The processing of certain instructions might take up to a minute.

The idea of my project was to create subrecepies for the cookingbot to have in its knowledge.
I decided to add as a subrecepie, one for doing a roux and one for doing a béchamel.

In order to achieve that, we use macros. They will allow us to expand a list of primitives to execute to do the subrecepies.
Each subrecepie needs a minimum amount of inputs, or the system lead to a no solution ending.

I created a 'subrecepie.fcg' file where to put the subrecepies in an initialized kitchen.

We find in that file :
 - a macro for each recepie
 - a function containing the list of primitives to be executed and expanded by the macro
 - some primitives dedicated to a specific subrecepie 
 - a function that expands macros in the list to be executed by the irl-program

 I also added the class roux and béchamel to the ontology.

 To be able to do the subrecepies, I changed some things in the files primitive.fcg and ontology.fcg :
 - I added some inheritance to the milk class to be manipulated in the recepie
 - I added the class saucepan
 - I created a new primitive "transfer-container" that transfers container from somewhere in the kitchen in a new destination
 - I added the cases where quantity and unit were given to the primitive transfer-content

 The details of the subrecepies are given in the functions.

 Some improvements to the work I've done :
 - Rename the homogeneous mixture at the end of each subrecepie the name of the class (e.g for roux, link the homogeneous mixture as an instance of the class roux)
 - Generalize "transfer-container" to transfer any object within the kitchen
 - Add much more subrecepies to developp the knowledge of the cookingrobots in terms of basic recepies (add some sweat subrecepies such as doing a pastry cream, a whipped cream, some "choux" etc.)

