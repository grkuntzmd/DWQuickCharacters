module Moves exposing (view)

import Html exposing (Html, div)
import Html.Attributes exposing (class, for, id, type_)
import Markdown


view : Html msg
view =
    div [ class "border border-primary mt-1 rounded row" ]
        [ div [ class "col-md-6 col-xl-3" ] [ Markdown.toHtml [] hackAndSlash ]
        , div [ class "col-md-6 col-xl-3" ] [ Markdown.toHtml [] volley ]
        , div [ class "col-md-6 col-xl-3" ] [ Markdown.toHtml [] aidOrInterfere ]
        , div [ class "col-md-6 col-xl-3" ] [ Markdown.toHtml [] defyDanger ]
        , div [ class "col-md-6 col-xl-3" ] [ Markdown.toHtml [] defend ]
        , div [ class "col-md-6 col-xl-3" ] [ Markdown.toHtml [] spoutLore ]
        , div [ class "col-md-6 col-xl-3" ] [ Markdown.toHtml [] discernRealities ]
        , div [ class "col-md-6 col-xl-3" ] [ Markdown.toHtml [] parley ]
        , div [ class "col-md-6 col-xl-3" ] [ Markdown.toHtml [] lastBreath ]
        ]


aidOrInterfere : String
aidOrInterfere =
    """
## Aid or Interfere

**When you help or hinder someone you
have a bond with,** roll+Bond with them.
On a 10+ they take +1 or -2, your choice.
On a 7–9 you also expose yourself to
danger, retribution, or cost.
"""


defend : String
defend =
    """
## Defend

**When you stand in defense of a person, item, or location
under attack,** roll+CON. On a 10+, hold 3. On a 7–9,
hold 1. So long as you stand in defense, when you or the
thing you defend is attacked you may spend hold, 1 for 1,
to choose an option:

* Redirect an attack from the thing you defend to
yourself
* Halve the attack’s effect or damage
* Open up the attacker to an ally giving that ally +1
forward against the attacker
* Deal damage to the attacker equal to your level
"""


defyDanger : String
defyDanger =
    """
## Defy Danger

**When you act despite an imminent threat or suffer a
calamity,** say how you deal with it and roll. If you do it…

* …by powering through, +STR
* …by getting out of the way or acting fast, +DEX
* …by enduring, +CON
* …with quick thinking, +INT
* …through mental fortitude, +WIS
* …using charm and social grace, +CHA
* ...using dumb luck, +nothing!

On a 10+, you do what you set out to, the threat doesn’t
come to bear. On a 7–9, you stumble, hesitate, or flinch:
the GM will offer you a worse outcome, hard bargain, or
ugly choice.
"""


discernRealities : String
discernRealities =
    """
## Discern Realities

**When you closely study a situation or
person,** roll+Wis. On a 10+ ask the GM 3
questions from the list below. On a 7–9
ask 1. Take +1 forward when acting on the
answers.

* What happened here recently?
* What is about to happen?
* What should I be on the lookout
for?
* What here is useful or valuable
to me?
* Who’s really in control here?
* What here is not what it appears
to be?
"""


hackAndSlash : String
hackAndSlash =
    """
## Hack and Slash

**When you attack an enemy up close,** roll+STR. On a hit,
you deal On a 10+ you deal your damage to the enemy
and avoid their attack. At your option, you may choose
to do +1d6 damage but expose yourself to the enemy’s
attack. On a 7–9, you deal your damage to the enemy and
the enemy makes an attack against you.
"""


lastBreath : String
lastBreath =
    """
## Last Breath

*Note: the following rule replaces the one
given in the **Dungeon World** rulebook.*

When you reach 0 hp, you black out. You
regain consciousness when the GM says
you do - you’ll be in a perilous situation,
on 1hp, at the mercy of your foes, with
none of your equipment. When (if!) you
reclaim your equipment, regain all your
HP - you’re back in the fight!
"""


parley : String
parley =
    """
## Parley

**When you you have leverage on a GM
character and manipulate them,** roll+Cha.
Leverage is something they need or want.
On a hit they ask you for something and
do it if you make them a promise first. On
a 7–9, they need some concrete assurance
of your promise, right now.
"""


spoutLore : String
spoutLore =
    """
## Spout Lore

**When you consult your accumulated
knowledge about something,** roll+INT.
On a 10+ the GM will tell you something
interesting and useful about the subject
relevant to your situation. On a 7–9 the
GM will only tell you something interesting—it’s
on you to make it useful. The GM
might ask you “How do you know this?”
Tell them the truth, now.
"""


volley : String
volley =
    """
## Volley

**When you take aim and shoot at an enemy at range,**
roll+DEX. On a 10+ you have a clear shot—deal your damage.
On a 7–9, choose one (whichever you choose you
deal your damage):

* You have to move to get the shot placing you in
danger of the GM’s choice
* You have to take what you can get: -1d6 damage
* You have to take several shots, reducing your
ammo by one.
"""
