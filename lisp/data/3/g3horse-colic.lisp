;% 1. Title: Horse Colic database
;%
;% 2. Source Information
;%    -- Creators: Mary McLeish & Matt Cecile
;% 	  	Department of Computer Science
;% 		University of Guelph
;% 		Guelph, Ontario, Canada N1G 2W1
;% 		mdmcleish@water.waterloo.edu
;%    -- Donor:    Will Taylor (taylor@pluto.arc.nasa.gov)
;%    -- Date:     8/6/89
;%
;% 3. Past Usage:
;%    -- Unknown
;%
;% 4. Relevant Information:
;%
;%    -- 2 data files
;%       -- horse-colic.data: 300 training instances
;%       -- horse-colic.test: 68 test instances
;%    -- Possible class attributes: 24 (whether lesion is surgical)
;%      -- others include: 23, 25, 26, and 27
;%    -- Many Data types: (continuous, discrete, and nominal)
;%
;% 5. Number of Instances: 368 (300 for training, 68 for testing)
;%
;% 6. Number of attributes: 28
;%
;% 7. Attribute Information:
;%
;%   1:  surgery?
;%           1 = Yes, it had surgery
;%           2 = It was treated without surgery
;%
;%   2:  Age
;%           1 = Adult horse
;%           2 = Young (< 6 months)
;%
;%   3:  Hospital Number
;%           - numeric id
;%           - the case number assigned to the horse
;%             (may not be unique if the horse is treated > 1 time)
;%
;%   4:  rectal temperature
;%           - linear
;%           - in degrees celsius.
;%           - An elevated temp may occur due to infection.
;%           - temperature may be reduced when the animal is in late shock
;%           - normal temp is 37.8
;%           - this parameter will usually change as the problem progresses
;%                eg. may start out normal, then become elevated because of
;%                    the lesion, passing back through the normal range as the
;%                    horse goes into shock
;%   5:  pulse
;%           - linear
;%           - the heart rate in beats per minute
;%           - is a reflection of the heart condition: 30 -40 is normal for adults
;%           - rare to have a lower than normal rate although athletic horses
;%             may have a rate of 20-25
;%           - animals with painful lesions or suffering from circulatory shock
;%             may have an elevated heart rate
;%
;%   6:  respiratory rate
;%           - linear
;%           - normal rate is 8 to 10
;%           - usefulness is doubtful due to the great fluctuations
;%
;%   7:  temperature of extremities
;%           - a subjective indication of peripheral circulation
;%           - possible values:
;%                1 = Normal
;%                2 = Warm
;%                3 = Cool
;%                4 = Cold
;%           - cool to cold extremities indicate possible shock
;%           - hot extremities should correlate with an elevated rectal temp.
;%
;%   8:  peripheral pulse
;%           - subjective
;%           - possible values are:
;%                1 = normal
;%                2 = increased
;%                3 = reduced
;%                4 = absent
;%           - normal or increased p.p. are indicative of adequate circulation
;%             while reduced or absent indicate poor perfusion
;%
;%   9:  mucous membranes
;%           - a subjective measurement of colour
;%           - possible values are:
;%                1 = normal pink
;%                2 = bright pink
;%                3 = pale pink
;%                4 = pale cyanotic
;%                5 = bright red / injected
;%                6 = dark cyanotic
;%           - 1 and 2 probably indicate a normal or slightly increased
;%             circulation
;%           - 3 may occur in early shock
;%           - 4 and 6 are indicative of serious circulatory compromise
;%           - 5 is more indicative of a septicemia
;%
;%  10: capillary refill time
;%           - a clinical judgement. The longer the refill, the poorer the
;%             circulation
;%           - possible values
;%                1 = < 3 seconds
;%                2 = >= 3 seconds
;%
;%  11: pain - a subjective judgement of the horse's pain level
;%           - possible values:
;%                1 = alert, no pain
;%                2 = depressed
;%                3 = intermittent mild pain
;%                4 = intermittent severe pain
;%                5 = continuous severe pain
;%           - should NOT be treated as a ordered or discrete variable!
;%           - In general, the more painful, the more likely it is to require
;%             surgery
;%           - prior treatment of pain may mask the pain level to some extent
;%
;%  12: peristalsis
;%           - an indication of the activity in the horse's gut. As the gut
;%             becomes more distended or the horse becomes more toxic, the
;%             activity decreases
;%           - possible values:
;%                1 = hypermotile
;%                2 = normal
;%                3 = hypomotile
;%                4 = absent
;%
;%  13: abdominal distension
;%           - An IMPORTANT parameter.
;%           - possible values
;%                1 = none
;%                2 = slight
;%                3 = moderate
;%                4 = severe
;%           - an animal with abdominal distension is likely to be painful and
;%             have reduced gut motility.
;%           - a horse with severe abdominal distension is likely to require
;%             surgery just tio relieve the pressure
;%
;%  14: nasogastric tube
;%           - this refers to any gas coming out of the tube
;%           - possible values:
;%                1 = none
;%                2 = slight
;%                3 = significant
;%           - a large gas cap in the stomach is likely to give the horse
;%             discomfort
;%
;%  15: nasogastric reflux
;%           - possible values
;%                1 = none
;%                2 = > 1 liter
;%                3 = < 1 liter
;%           - the greater amount of reflux, the more likelihood that there is
;%             some serious obstruction to the fluid passage from the rest of
;%             the intestine
;%
;%  16: nasogastric reflux PH
;%           - linear
;%           - scale is from 0 to 14 with 7 being neutral
;%           - normal values are in the 3 to 4 range
;%
;%  17: rectal examination - feces
;%           - possible values
;%                1 = normal
;%                2 = increased
;%                3 = decreased
;%                4 = absent
;%           - absent feces probably indicates an obstruction
;%
;%  18: abdomen
;%           - possible values
;%                1 = normal
;%                2 = other
;%                3 = firm feces in the large intestine
;%                4 = distended small intestine
;%                5 = distended large intestine
;%           - 3 is probably an obstruction caused by a mechanical impaction
;%             and is normally treated medically
;%           - 4 and 5 indicate a surgical lesion
;%
;%  19: packed cell volume
;%           - linear
;%           - the # of red cells by volume in the blood
;%           - normal range is 30 to 50. The level rises as the circulation
;%             becomes compromised or as the animal becomes dehydrated.
;%
;%  20: total protein
;%           - linear
;%           - normal values lie in the 6-7.5 (gms/dL) range
;%           - the higher the value the greater the dehydration
;%
;%  21: abdominocentesis appearance
;%           - a needle is put in the horse's abdomen and fluid is obtained from
;%             the abdominal cavity
;%           - possible values:
;%                1 = clear
;%                2 = cloudy
;%                3 = serosanguinous
;%           - normal fluid is clear while cloudy or serosanguinous indicates
;%             a compromised gut
;%
;%  22: abdomcentesis total protein
;%           - linear
;%           - the higher the level of protein the more likely it is to have a
;%             compromised gut. Values are in gms/dL
;%
;%  23: outcome
;%           - what eventually happened to the horse?
;%           - possible values:
;%                1 = lived
;%                2 = died
;%                3 = was euthanized
;%
;%  24: surgical lesion?
;%           - retrospectively, was the problem (lesion) surgical?
;%           - all cases are either operated upon or autopsied so that
;%             this value and the lesion type are always known
;%           - possible values:
;%                1 = Yes
;%                2 = No
;%
;%
;%
;%
;%
;% Relabeled values in attribute 'surgery'
;%    From: '1'                     To: yes
;%    From: '2'                     To: no
;%
;%
;% Relabeled values in attribute 'Age'
;%    From: '1'                     To: adult
;%    From: '9'                     To: young
;%
;%
;% Relabeled values in attribute 'temp_extremities'
;%    From: '1'                     To: normal
;%    From: '2'                     To: warm
;%    From: '3'                     To: cool
;%    From: '4'                     To: cold
;%
;%
;% Relabeled values in attribute 'peripheral_pulse'
;%    From: '1'                     To: normal
;%    From: '2'                     To: increased
;%    From: '3'                     To: reduced
;%    From: '4'                     To: absent
;%
;%
;% Relabeled values in attribute 'mucous_membranes'
;%    From: '1'                     To: normal-pink
;%    From: '2'                     To: bright-pink
;%    From: '3'                     To: pale-pink
;%    From: '4'                     To: pale-cyanotic
;%    From: '5'                     To: bright-red
;%    From: '6'                     To: dark-cyanotic
;%
;%
;% Relabeled values in attribute 'capillary_refill_time'
;%    From: '1'                     To: <3
;%    From: '2'                     To: >=3
;%    From: '3'                     To: meaning-unknown
;%
;%
;% Relabeled values in attribute 'pain'
;%    From: '1'                     To: alert-no-pain
;%    From: '2'                     To: depressed
;%    From: '3'                     To: intermittent-mild-pain
;%    From: '4'                     To: intermittent-severe-pain
;%    From: '5'                     To: continuous-severe-pain
;%
;%
;% Relabeled values in attribute 'peristalsis'
;%    From: '1'                     To: hypermotile
;%    From: '2'                     To: normal
;%    From: '3'                     To: hypomotile
;%    From: '4'                     To: absent
;%
;%
;% Relabeled values in attribute 'abdominal_distension'
;%    From: '1'                     To: none
;%    From: '2'                     To: slight
;%    From: '3'                     To: moderate
;%    From: '4'                     To: severe
;%
;%
;% Relabeled values in attribute 'nasogastric_tube'
;%    From: '1'                     To: none
;%    From: '2'                     To: slight
;%    From: '3'                     To: significant
;%
;%
;% Relabeled values in attribute 'nasogastric_reflux'
;%    From: '1'                     To: none
;%    From: '2'                     To: >11
;%    From: '3'                     To: <11
;%
;%
;% Relabeled values in attribute 'rectal_examination'
;%    From: '1'                     To: normal
;%    From: '2'                     To: increased
;%    From: '3'                     To: decreased
;%    From: '4'                     To: absent
;%
;%
;% Relabeled values in attribute 'abdomen'
;%    From: '1'                     To: normal
;%    From: '2'                     To: other
;%    From: '3'                     To: firm-feces-in-large-intestine
;%    From: '4'                     To: distended-small-intestine
;%    From: '5'                     To: distended-large-intestine
;%
;%
;% Relabeled values in attribute 'abdominocentesis_appearance'
;%    From: '1'                     To: clear
;%    From: '2'                     To: cloudy
;%    From: '3'                     To: serosanguinous
;%
;%
;% Relabeled values in attribute 'outcome'
;%    From: '1'                     To: lived
;%    From: '2'                     To: died
;%    From: '3'                     To: euthanized
;%
;%
;% Relabeled values in attribute 'surgical_lesion'
;%    From: '1'                     To: yes
;%    From: '2'                     To: no
;%
(defun g3horse-colic ()
  (data
    :name 'g3horse-colic
    :columns '(surgery Age $rectal_temperature $pulse $respiratory_rate temp_extremities peripheral_pulse mucous_membranes capillary_refill_time pain peristalsis abdominal_distension nasogastric_tube nasogastric_reflux $nasogastric_reflux_PH rectal_examination abdomen $packed_cell_volume $total_protein abdominocentesis_appearance $abdomcentesis_total_protein outcome surgical_lesion)
    :egs
    '(
      (no adult 38.5 66 28 cool reduced ? >=3 continuous-severe-pain absent severe ? ? ? decreased distended-large-intestine 45 8.4 ? ? died no)
      (yes adult 39.2 88 20 ? ? pale-cyanotic <3 intermittent-mild-pain absent slight ? ? ? absent other 50 85 cloudy 2 euthanized no)
      (no adult 38.3 40 24 normal normal pale-pink <3 intermittent-mild-pain hypomotile none ? ? ? normal normal 33 6.7 ? ? lived no)
      (yes young 39.1 164 84 cold normal dark-cyanotic >=3 depressed absent severe none >11 5 decreased ? 48 7.2 serosanguinous 5.3 died yes)
      (no adult 37.3 104 35 ? ? dark-cyanotic >=3 ? ? ? ? ? ? ? ? 74 7.4 ? ? died no)
      (no adult ? ? ? warm normal pale-pink <3 depressed hypomotile slight slight none ? decreased firm-feces-in-large-intestine ? ? ? ? lived no)
      (yes adult 37.9 48 16 normal normal normal-pink <3 intermittent-mild-pain hypomotile moderate none none ? decreased distended-large-intestine 37 7 ? ? lived yes)
      (yes adult ? 60 ? cool ? ? <3 ? absent slight slight none ? decreased distended-small-intestine 44 8.3 ? ? died yes)
      (no adult ? 80 36 cool absent pale-pink <3 intermittent-severe-pain absent severe slight none ? decreased distended-large-intestine 38 6.2 ? ? euthanized yes)
      (no young 38.3 90 ? normal ? normal-pink <3 continuous-severe-pain hypomotile none slight none ? decreased ? 40 6.2 clear 2.2 lived no)
      (yes adult 38.1 66 12 cool reduced bright-red <3 intermittent-mild-pain hypomotile none slight none 3 increased distended-large-intestine 44 6 cloudy 3.6 lived yes)
      (no adult 39.1 72 52 warm ? bright-pink <3 depressed hypermotile slight none none ? absent distended-small-intestine 50 7.8 ? ? lived yes)
      (yes adult 37.2 42 12 warm normal normal-pink <3 intermittent-mild-pain hypomotile moderate significant none ? absent distended-large-intestine ? 7 ? ? lived no)
      (no young 38 92 28 normal normal bright-pink <3 alert-no-pain hypomotile slight significant ? 7.2 normal normal 37 6.1 clear ? died no)
      (yes adult 38.2 76 28 cool normal normal-pink <3 intermittent-mild-pain absent none slight >11 ? absent distended-small-intestine 46 81 clear 2 lived yes)
      (yes adult 37.6 96 48 cool normal pale-cyanotic <3 continuous-severe-pain hypomotile moderate slight <11 4.5 absent ? 45 6.8 ? ? died yes)
      (yes young ? 128 36 cool reduced pale-cyanotic >=3 intermittent-severe-pain absent moderate significant ? ? absent distended-large-intestine 53 7.8 serosanguinous 4.7 died no)
      (no adult 37.5 48 24 ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? lived no)
      (yes adult 37.6 64 21 normal normal bright-pink <3 depressed hypomotile none none none ? increased distended-large-intestine 40 7 clear ? lived yes)
      (no adult 39.4 110 35 cold reduced dark-cyanotic ? ? hypomotile moderate ? ? ? ? ? 55 8.7 ? ? lived no)
      (yes adult 39.9 72 60 normal normal bright-red >=3 continuous-severe-pain absent severe significant none ? absent distended-small-intestine 46 6.1 cloudy ? lived yes)
      (no adult 38.4 48 16 normal ? normal-pink <3 alert-no-pain hypomotile none slight <11 5.5 absent firm-feces-in-large-intestine 49 6.8 ? ? lived no)
      (yes adult 38.6 42 34 warm normal pale-cyanotic ? depressed hypomotile none ? ? ? normal ? 48 7.2 ? ? lived yes)
      (yes young 38.3 130 60 ? reduced ? <3 depressed absent ? ? ? ? ? ? 50 70 ? ? lived yes)
      (yes adult 38.1 60 12 cool reduced pale-pink <3 ? absent moderate significant >11 2 ? ? 51 65 ? ? lived yes)
      (no adult 37.8 60 42 ? ? ? <3 ? ? ? ? ? ? ? ? ? ? ? ? lived no)
      (yes adult 38.3 72 30 cold reduced pale-pink >=3 intermittent-mild-pain hypomotile moderate slight none ? decreased distended-large-intestine 43 7 cloudy 3.9 lived yes)
      (yes adult 37.8 48 12 cool normal normal-pink <3 ? hypomotile slight none none ? normal firm-feces-in-large-intestine 37 5.5 cloudy 1.3 lived no)
      (yes adult ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? died no)
      (no adult 37.7 48 ? warm normal normal-pink <3 alert-no-pain hypermotile none none none ? ? ? 45 76 ? ? lived no)
      (no adult 37.7 96 30 cool reduced pale-cyanotic >=3 continuous-severe-pain absent severe significant >11 4 absent distended-large-intestine 66 7.5 ? ? died yes)
      (no adult 37.2 108 12 cool reduced pale-cyanotic >=3 depressed absent slight ? <11 6 decreased firm-feces-in-large-intestine 52 8.2 serosanguinous 7.4 euthanized yes)
      (yes adult 37.2 60 ? warm normal normal-pink <3 intermittent-mild-pain hypomotile moderate slight none ? absent distended-large-intestine 43 6.6 ? ? lived yes)
      (yes adult 38.2 64 28 normal normal normal-pink <3 intermittent-mild-pain hypermotile ? ? ? ? absent distended-small-intestine 49 8.6 cloudy 6.6 lived yes)
      (yes adult ? 100 30 cool reduced pale-cyanotic >=3 continuous-severe-pain absent severe significant <11 ? absent distended-small-intestine 52 6.6 ? ? lived yes)
      (no adult ? 104 24 cold reduced pale-pink >=3 intermittent-severe-pain absent moderate ? <11 ? ? other 73 8.4 ? ? euthanized yes)
      (no adult 38.3 112 16 ? reduced bright-red >=3 ? ? none none >11 ? ? distended-large-intestine 51 6 cloudy 1 euthanized no)
      (yes adult 37.8 72 ? ? reduced ? <3 continuous-severe-pain hypomotile none ? none ? normal normal 56 80 clear 2 lived yes)
      (no adult 38.6 52 ? normal normal normal-pink <3 intermittent-mild-pain hypomotile slight none none ? normal firm-feces-in-large-intestine 32 6.6 clear 5 lived no)
      (yes young 39.2 146 96 ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? died yes)
      (yes adult ? 88 ? cool reduced dark-cyanotic >=3 continuous-severe-pain hypomotile moderate none <11 ? absent distended-large-intestine 63 6.5 serosanguinous ? died yes)
      (no young 39 150 72 ? ? ? ? ? ? ? ? ? ? ? ? 47 8.5 ? 0.1 lived yes)
      (no adult 38 60 12 cool normal pale-pink <3 intermittent-mild-pain hypomotile none none none ? increased other 47 7 ? ? lived no)
      (yes adult ? 120 ? cool absent pale-cyanotic <3 intermittent-severe-pain absent severe none none ? ? distended-large-intestine 52 67 cloudy 2 euthanized yes)
      (yes adult 35.4 140 24 cool reduced pale-cyanotic >=3 intermittent-severe-pain absent ? slight none ? ? distended-large-intestine 57 69 serosanguinous 2 euthanized yes)
      (no adult ? 120 ? cold reduced pale-cyanotic >=3 continuous-severe-pain absent severe none none ? absent distended-large-intestine 60 6.5 serosanguinous ? died yes)
      (yes adult 37.9 60 15 cool ? pale-cyanotic >=3 continuous-severe-pain absent severe slight >11 ? absent distended-large-intestine 65 7.5 ? ? lived yes)
      (no adult 37.5 48 16 normal normal normal-pink <3 alert-no-pain hypermotile none none none ? normal ? 37 6.5 ? ? lived no)
      (yes adult 38.9 80 44 cool reduced pale-pink >=3 depressed hypomotile moderate slight >11 7 decreased normal 54 6.5 serosanguinous ? died yes)
      (no adult 37.2 84 48 cool reduced bright-red >=3 intermittent-severe-pain hypermotile slight none >11 ? increased normal 73 5.5 cloudy 4.1 died no)
      (no adult 38.6 46 ? normal normal bright-pink <3 alert-no-pain hypomotile slight none none ? ? other 49 9.1 clear 1.6 lived no)
      (yes adult 37.4 84 36 normal ? pale-pink >=3 intermittent-mild-pain hypomotile slight ? ? ? absent distended-large-intestine ? ? serosanguinous ? died yes)
      (no adult ? ? ? normal normal pale-pink <3 alert-no-pain hypomotile none ? ? ? increased other 43 7.7 ? ? lived no)
      (no adult 38.6 40 20 ? ? ? <3 ? ? ? ? ? ? ? ? 41 6.4 ? ? lived no)
      (no adult 40.3 114 36 cool reduced normal-pink >=3 depressed hypomotile moderate slight none 7 normal distended-large-intestine 57 8.1 serosanguinous 4.5 euthanized yes)
      (yes young 38.6 160 20 cool ? bright-red <3 intermittent-mild-pain hypomotile severe significant ? ? absent ? 38 ? cloudy ? died yes)
      (yes adult ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? 24 6.7 ? ? lived yes)
      (yes adult ? 64 36 warm ? bright-pink <3 continuous-severe-pain hypomotile moderate slight >11 ? ? ? 42 7.7 ? ? died yes)
      (yes adult ? ? 20 cold reduced pale-pink ? continuous-severe-pain absent moderate slight ? ? absent distended-small-intestine 53 5.9 serosanguinous ? died yes)
      (no adult ? 96 ? cool reduced pale-pink >=3 continuous-severe-pain absent severe none >11 ? absent distended-large-intestine 60 ? ? ? died yes)
      (no adult 37.8 48 32 normal normal pale-pink <3 depressed hypermotile ? none none ? absent distended-large-intestine 37 6.7 ? ? lived no)
      (no adult 38.5 60 ? warm increased normal-pink <3 alert-no-pain normal slight slight none ? normal normal 44 7.7 ? ? lived no)
      (yes adult 37.8 88 22 warm normal bright-pink <3 intermittent-mild-pain ? ? slight ? ? absent ? 64 8 clear 6 died yes)
      (no adult 38.2 130 16 cold reduced pale-cyanotic >=3 depressed absent severe none none ? ? ? 65 82 cloudy 2 euthanized no)
      (yes adult 39 64 36 cool normal pale-cyanotic >=3 intermittent-mild-pain hypomotile slight none >11 7 absent distended-large-intestine 44 7.5 serosanguinous 5 lived yes)
      (yes adult ? 60 36 cool normal pale-pink <3 intermittent-mild-pain hypomotile slight none none ? decreased distended-small-intestine 26 72 cloudy 1 lived yes)
      (no adult 37.9 72 ? normal normal bright-red >=3 intermittent-mild-pain hypomotile none none <11 2 decreased distended-small-intestine 58 74 clear 2 lived yes)
      (no adult 38.4 54 24 normal normal normal-pink <3 alert-no-pain hypomotile none slight none ? decreased other 49 7.2 clear ? lived no)
      (no adult ? 52 16 normal ? pale-pink <3 ? ? ? slight <11 5.5 ? ? 55 7.2 ? ? lived no)
      (no adult 38 48 12 normal normal normal-pink <3 alert-no-pain hypomotile ? none none ? decreased other 42 6.3 cloudy 4.1 lived no)
      (no adult 37 60 20 cool ? ? <3 intermittent-mild-pain ? moderate slight >11 4.5 absent distended-small-intestine 43 7.6 ? ? euthanized yes)
      (yes adult 37.8 48 28 normal normal normal-pink <3 alert-no-pain normal none slight ? ? normal normal 46 5.9 cloudy 7 lived no)
      (yes adult 37.7 56 ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? died yes)
      (yes adult 38.1 52 24 normal normal bright-red <3 intermittent-severe-pain hypomotile none slight <11 7 normal ? 54 7.5 cloudy 2.6 died yes)
      (yes young ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? 37 4.9 ? ? died yes)
      (yes young 39.7 100 ? cool reduced bright-red >=3 depressed hypomotile ? ? ? ? ? ? 48 57 cloudy 2 euthanized yes)
      (yes adult 37.6 38 20 cool reduced normal-pink <3 intermittent-mild-pain hypomotile slight ? ? ? decreased ? 37 68 ? ? lived yes)
      (no adult 38.7 52 20 warm ? normal-pink <3 alert-no-pain hypermotile none none none ? normal normal 33 77 ? ? lived no)
      (yes adult ? ? ? cool reduced pale-pink meaning-unknown continuous-severe-pain hypomotile moderate significant >11 ? absent distended-large-intestine 46 5.9 ? ? died yes)
      (yes adult 37.5 96 18 normal reduced dark-cyanotic >=3 intermittent-mild-pain absent slight slight <11 5 ? distended-small-intestine 69 8.9 serosanguinous ? lived yes)
      (yes adult 36.4 98 35 cool reduced pale-cyanotic <3 intermittent-severe-pain hypomotile slight ? ? ? absent distended-small-intestine 47 6.4 serosanguinous 3.6 died yes)
      (yes adult 37.3 40 ? ? reduced normal-pink <3 depressed hypomotile slight significant none ? decreased distended-large-intestine 36 ? serosanguinous 2 lived yes)
      (yes young 38.1 100 80 cool normal bright-pink <3 intermittent-mild-pain absent none ? ? ? normal ? 36 5.7 ? ? lived yes)
      (yes adult 38 ? 24 cool reduced dark-cyanotic >=3 continuous-severe-pain ? severe none none ? ? ? 68 7.8 ? ? died yes)
      (yes adult 37.8 60 80 normal reduced bright-pink >=3 depressed hypomotile moderate ? >11 5.5 absent ? 40 4.5 cloudy ? lived yes)
      (no adult 38 54 30 warm reduced pale-pink meaning-unknown intermittent-mild-pain hypermotile slight slight >11 ? ? distended-small-intestine 45 6.2 ? ? lived no)
      (yes adult ? 88 40 cool reduced pale-cyanotic >=3 continuous-severe-pain absent moderate significant ? ? absent distended-large-intestine 50 7.7 serosanguinous 1.4 died yes)
      (no adult ? 40 16 ? ? ? <3 ? ? ? ? ? ? ? ? 50 7 cloudy 3.9 euthanized yes)
      (no adult 39 64 40 normal normal bright-red <3 intermittent-mild-pain hypomotile slight slight none ? decreased firm-feces-in-large-intestine 42 7.5 cloudy 2.3 lived no)
      (no adult 38.3 42 10 normal normal normal-pink <3 alert-no-pain hypermotile none ? ? ? ? ? 38 61 ? ? lived no)
      (no adult 38 52 16 ? ? ? ? depressed ? ? ? <11 1 normal normal 53 86 ? ? lived yes)
      (no adult 40.3 114 36 cool reduced normal-pink >=3 depressed hypomotile moderate slight none 7 normal distended-large-intestine 57 8.1 serosanguinous 4.5 died yes)
      (no adult 38.8 50 20 cool normal normal-pink <3 alert-no-pain hypermotile none slight none ? decreased normal 42 6.2 ? ? lived no)
      (no adult ? ? ? cool reduced normal-pink <3 continuous-severe-pain hypomotile moderate none none ? absent distended-large-intestine 38 6.5 ? ? died yes)
      (no adult 37.5 48 30 cold normal pale-pink <3 ? normal none none none ? normal normal 48 8.6 ? ? lived no)
      (yes adult 37.3 48 20 ? normal bright-pink <3 intermittent-mild-pain hypomotile moderate slight none ? decreased distended-large-intestine 41 69 ? ? lived yes)
      (no adult ? 84 36 ? ? pale-pink <3 ? hypomotile none slight none ? decreased other 44 8.5 ? ? lived yes)
      (yes adult 38.1 88 32 cool reduced pale-cyanotic <3 depressed hypomotile moderate ? <11 1 absent distended-large-intestine 55 60 ? ? euthanized no)
      (no adult 37.7 44 40 warm normal pale-pink <3 alert-no-pain hypomotile slight none none ? normal distended-large-intestine 41 60 ? ? lived no)
      (no adult 39.6 108 51 cool reduced dark-cyanotic >=3 depressed absent moderate none >11 ? decreased distended-large-intestine 59 8 cloudy 2.6 lived no)
      (yes adult 38.2 40 16 cool reduced normal-pink <3 alert-no-pain hypomotile ? ? ? ? normal normal 34 66 ? ? lived no)
      (yes adult ? 60 20 cold reduced pale-cyanotic >=3 continuous-severe-pain absent ? ? none ? absent distended-large-intestine ? ? ? ? euthanized yes)
      (no adult 38.3 40 16 cool ? normal-pink <3 depressed ? ? ? ? ? ? ? 37 57 ? ? lived no)
      (yes young 38 140 68 normal normal normal-pink <3 intermittent-mild-pain hypomotile slight ? ? ? increased normal 39 5.3 ? ? lived yes)
      (yes adult 37.8 52 24 normal reduced pale-pink <3 intermittent-severe-pain absent none slight <11 5.7 increased distended-large-intestine 48 6.6 clear 3.7 died yes)
      (yes adult ? 70 36 normal ? pale-pink >=3 depressed hypomotile slight slight ? ? absent distended-large-intestine 36 7.3 ? ? lived yes)
      (yes adult 38.3 52 96 ? reduced pale-pink <3 ? ? ? none none ? normal ? 43 6.1 ? ? lived yes)
      (no adult 37.3 50 32 normal normal pale-pink <3 alert-no-pain hypomotile slight ? ? ? normal ? 44 7 ? ? lived no)
      (yes adult 38.7 60 32 cold reduced bright-pink >=3 intermittent-severe-pain absent severe ? ? ? absent distended-large-intestine 53 64 serosanguinous 2 euthanized yes)
      (yes young 38.4 84 40 cool reduced bright-pink <3 intermittent-mild-pain hypomotile moderate none none ? ? ? 36 6.6 cloudy 2.8 died yes)
      (yes adult ? 70 16 cool absent bright-red >=3 depressed hypomotile slight slight none ? absent distended-large-intestine 60 7.5 ? ? died yes)
      (yes adult 38.3 40 16 cool ? ? <3 alert-no-pain hypomotile slight ? ? ? ? ? 38 58 clear 2 lived yes)
      (yes adult ? 40 ? warm normal normal-pink <3 alert-no-pain hypomotile none none none ? ? distended-large-intestine 39 56 ? ? lived yes)
      (yes adult 36.8 60 28 ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? 10 died yes)
      (yes adult 38.4 44 24 cool ? pale-cyanotic ? continuous-severe-pain absent moderate slight none ? absent distended-large-intestine 50 77 ? ? lived yes)
      (no adult ? ? 40 cool normal normal-pink <3 intermittent-mild-pain hypomotile slight ? ? ? ? ? 45 70 ? ? lived no)
      (yes adult 38 44 12 normal normal normal-pink <3 intermittent-mild-pain hypomotile moderate slight none ? absent distended-large-intestine 42 65 ? ? lived yes)
      (no adult 39.5 ? ? cool reduced pale-cyanotic >=3 intermittent-mild-pain absent moderate ? <11 5.5 absent distended-large-intestine ? 6.7 clear ? euthanized yes)
      (yes adult 36.5 78 30 normal ? normal-pink <3 continuous-severe-pain hypomotile none ? none ? ? ? 34 75 cloudy 1 lived yes)
      (no adult 38.1 56 20 warm normal bright-pink <3 alert-no-pain hypomotile none none none ? ? ? 46 70 ? ? lived no)
      (yes adult 39.4 54 66 normal normal bright-pink <3 depressed hypomotile slight none none ? decreased distended-small-intestine 39 6 cloudy ? lived yes)
      (yes adult 38.3 80 40 ? ? dark-cyanotic >=3 intermittent-severe-pain hypomotile none ? >11 ? normal distended-small-intestine 67 10.2 cloudy 1 euthanized yes)
      (no adult 38.7 40 28 warm normal normal-pink <3 intermittent-mild-pain hypermotile none ? ? ? normal ? 39 62 clear 1 lived no)
      (yes adult 38.2 64 24 normal normal pale-pink <3 intermittent-severe-pain absent moderate slight none ? absent distended-small-intestine 45 7.5 clear 2 died yes)
      (no adult 37.6 48 20 cool normal pale-cyanotic <3 alert-no-pain hypermotile moderate slight none ? normal normal 37 5.5 ? ? euthanized yes)
      (yes adult 38 42 68 cold normal normal-pink <3 intermittent-mild-pain hypomotile slight slight >11 ? absent distended-small-intestine 41 7.6 ? ? lived yes)
      (yes adult 38.7 ? ? cool normal pale-pink <3 continuous-severe-pain absent slight ? ? ? ? ? 33 6.5 cloudy ? lived yes)
      (yes adult 37.4 50 32 cool reduced ? <3 intermittent-severe-pain absent none slight none ? normal ? 45 7.9 cloudy 1 lived yes)
      (yes adult 37.4 84 20 ? ? pale-pink <3 depressed hypomotile moderate ? ? ? ? ? 31 61 ? 1 euthanized no)
      (yes adult 38.4 49 ? ? ? normal-pink <3 ? ? none slight none ? ? ? 44 7.6 ? ? lived yes)
      (yes adult 37.8 30 12 ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? died yes)
      (no adult 37.6 88 36 cool normal normal-pink <3 intermittent-mild-pain hypomotile slight none <11 1.5 ? ? 44 6 ? ? died yes)
      (? adult 38 48 20 cool normal pale-pink <3 intermittent-severe-pain hypomotile none slight none ? decreased distended-large-intestine 43 73 cloudy 1 ? yes)
      (no adult 37.9 40 24 normal normal normal-pink <3 depressed hypomotile none ? ? ? ? firm-feces-in-large-intestine 40 5.7 ? ? lived yes)
      (yes adult ? 100 ? cool ? pale-cyanotic >=3 continuous-severe-pain absent ? slight ? ? increased ? 59 6.3 ? ? died yes)
      (yes young 38.1 136 48 cool reduced pale-pink <3 continuous-severe-pain hypermotile moderate slight >11 4.4 increased ? 33 4.9 cloudy 2.9 died yes)
      (yes adult ? ? ? cool reduced pale-pink >=3 continuous-severe-pain hypomotile moderate significant >11 ? absent distended-large-intestine 46 5.9 ? ? died yes)
      (yes adult 38 48 ? normal normal normal-pink <3 alert-no-pain normal severe slight >11 ? absent distended-large-intestine ? ? ? ? lived yes)
      (no adult 38 56 ? normal increased pale-pink <3 alert-no-pain hypermotile none none none ? normal normal 42 71 ? ? lived no)
      (no adult 38 60 32 normal normal ? <3 intermittent-mild-pain hypomotile ? none none ? ? ? 50 7 clear 1 lived no)
      (yes adult 38.1 44 9 cool normal normal-pink <3 depressed normal none none none ? absent distended-large-intestine 31 7.3 ? ? lived no)
      (no adult 36 42 30 ? ? bright-red <3 ? ? ? ? ? ? ? ? 64 6.8 ? ? died no)
      (yes adult ? 120 ? cold reduced dark-cyanotic >=3 continuous-severe-pain absent severe ? ? ? absent distended-large-intestine 57 4.5 serosanguinous 3.9 died yes)
      (yes adult 37.8 48 28 normal normal normal-pink >=3 alert-no-pain normal none slight ? ? normal normal 46 5.9 cloudy 7 lived no)
      (yes adult 37.1 84 40 cool reduced dark-cyanotic <3 depressed absent severe significant >11 2 absent distended-large-intestine 75 81 ? ? euthanized no)
      (no adult ? 80 32 cool reduced bright-pink <3 depressed hypomotile moderate slight none ? decreased ? 50 80 ? ? lived yes)
      (yes adult 38.2 48 ? normal reduced pale-pink <3 intermittent-mild-pain absent severe none <11 2 absent distended-large-intestine 42 71 ? ? lived yes)
      (no adult 38 44 12 warm normal pale-pink <3 intermittent-mild-pain absent moderate none >11 6.5 normal distended-small-intestine 33 6.5 ? ? died yes)
      (yes adult 38.3 132 ? ? reduced dark-cyanotic >=3 depressed absent slight slight <11 6.2 absent distended-small-intestine 57 8 ? 5.2 lived yes)
      (no adult 38.7 48 24 ? ? ? ? alert-no-pain hypermotile ? none none ? normal ? 34 63 ? ? lived no)
      (no adult 38.9 44 14 cool normal normal-pink <3 depressed hypomotile slight ? ? ? ? other 33 64 ? ? lived no)
      (yes adult 39.3 ? ? cold reduced dark-cyanotic >=3 intermittent-severe-pain absent slight none <11 4 absent distended-small-intestine 75 ? serosanguinous 4.3 died yes)
      (yes adult ? 100 ? cool reduced pale-cyanotic >=3 ? absent severe slight none 2 ? ? 68 64 serosanguinous 2 lived yes)
      (no adult 38.6 48 20 cool normal normal-pink <3 alert-no-pain hypomotile slight slight none ? decreased other 50 7.3 clear ? lived no)
      (no adult 38.8 48 40 normal normal pale-pink <3 intermittent-mild-pain hypomotile severe slight ? ? ? distended-large-intestine 41 65 ? ? lived yes)
      (no adult 38 48 20 cool reduced pale-cyanotic <3 alert-no-pain absent slight slight ? 5 ? other 49 8.3 clear ? lived no)
      (no adult 38.6 52 20 normal normal normal-pink <3 intermittent-mild-pain hypomotile slight none none ? normal firm-feces-in-large-intestine 36 6.6 clear 5 lived no)
      (yes adult 37.8 60 24 normal ? pale-pink >=3 ? absent severe slight <11 2 ? distended-large-intestine 52 75 ? ? euthanized yes)
      (no adult 38 42 40 cool normal normal-pink <3 intermittent-mild-pain hypomotile none ? ? ? ? ? ? ? ? ? lived no)
      (no adult ? ? 12 normal normal bright-pink <3 depressed hypermotile slight significant none ? normal firm-feces-in-large-intestine 44 7.5 cloudy ? lived no)
      (yes adult ? ? ? ? ? ? ? intermittent-severe-pain ? ? none none ? ? distended-large-intestine 35 58 cloudy 1 lived yes)
      (yes adult 38.3 42 24 ? ? ? <3 ? ? ? ? ? ? ? ? 40 8.5 ? ? died yes)
      (no adult 39.5 60 10 cool ? ? >=3 intermittent-mild-pain hypomotile slight slight none ? decreased ? 38 56 clear ? lived no)
      (yes adult 38 66 20 normal reduced pale-pink <3 continuous-severe-pain hypomotile none none none ? decreased ? 46 46 serosanguinous 2 euthanized yes)
      (yes adult 38.7 76 ? normal normal bright-red >=3 intermittent-mild-pain hypomotile slight slight >11 ? absent distended-small-intestine 50 8 ? ? lived yes)
      (yes adult 39.4 120 48 ? ? bright-red <3 ? hypomotile moderate none ? ? absent ? 56 64 clear 2 euthanized no)
      (yes adult 38.3 40 18 normal normal normal-pink <3 intermittent-mild-pain hypermotile none ? ? ? increased normal 43 5.9 clear ? lived no)
      (no adult ? 44 24 normal normal normal-pink <3 intermittent-mild-pain hypomotile none slight none ? ? normal ? 6.3 ? ? lived no)
      (yes adult 38.4 104 40 normal normal pale-pink <3 depressed absent slight slight <11 6.5 ? distended-small-intestine 55 8.5 ? ? lived yes)
      (yes adult ? 65 24 ? ? ? >=3 continuous-severe-pain ? severe significant none ? ? distended-large-intestine ? ? ? ? euthanized yes)
      (no adult 37.5 44 20 normal normal pale-pink <3 ? hypermotile none ? ? ? normal ? 35 7.2 ? ? lived no)
      (no adult 39 86 16 cool reduced bright-red ? intermittent-mild-pain hypomotile moderate ? >11 ? ? ? 68 5.8 serosanguinous 6 died yes)
      (yes adult 38.5 129 48 cool reduced pale-pink <3 depressed absent moderate none <11 2 ? ? 57 66 serosanguinous 2 lived yes)
      (yes adult ? 104 ? cool reduced bright-red >=3 depressed absent moderate ? <11 ? absent distended-small-intestine 69 8.6 cloudy 3.4 died yes)
      (no adult ? ? ? cool absent dark-cyanotic ? intermittent-severe-pain ? severe ? ? ? ? ? ? ? ? ? died yes)
      (yes adult ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? lived yes)
      (yes adult 38.2 60 30 normal normal pale-pink <3 intermittent-mild-pain hypomotile none slight none ? decreased other 48 66 ? ? lived yes)
      (yes adult ? 68 14 ? ? pale-cyanotic <3 intermittent-severe-pain ? ? ? none 4.3 ? ? ? ? cloudy 2.8 died yes)
      (yes adult ? 60 30 cool reduced pale-cyanotic >=3 continuous-severe-pain absent severe none none ? absent ? 45 70 serosanguinous 2 lived yes)
      (no adult 38.5 100 ? cool reduced bright-red >=3 intermittent-severe-pain hypomotile severe slight none ? absent distended-large-intestine ? ? ? ? euthanized no)
      (yes adult 38.4 84 30 cool normal bright-red >=3 intermittent-severe-pain hypomotile moderate slight <11 6.5 absent distended-small-intestine 47 7.5 serosanguinous ? died yes)
      (no adult 37.8 48 14 ? ? normal-pink <3 intermittent-mild-pain ? slight none <11 5.3 normal ? 35 7.5 ? ? lived no)
      (yes adult 38 ? 24 cool reduced dark-cyanotic >=3 continuous-severe-pain ? severe none none ? ? ? 68 7.8 ? ? died yes)
      (no adult 37.8 56 16 normal normal bright-pink <3 depressed hypermotile none slight none ? normal ? 44 68 clear 1 lived no)
      (no adult 38.2 68 32 warm increased bright-pink <3 alert-no-pain hypermotile none significant none ? normal normal 43 65 ? ? lived no)
      (yes adult 38.5 120 60 cold reduced dark-cyanotic >=3 ? hypomotile ? ? ? ? ? ? 54 ? ? ? lived yes)
      (yes adult 39.3 64 90 warm reduced normal-pink <3 ? hypomotile none none >11 ? ? ? 39 6.7 ? ? lived yes)
      (yes adult 38.4 80 30 cold reduced normal-pink <3 intermittent-mild-pain hypomotile moderate significant <11 ? absent distended-large-intestine 32 6.1 serosanguinous 4.3 lived yes)
      (yes adult 38.5 60 ? normal normal ? <3 ? hypermotile none ? ? ? ? ? 33 53 clear ? lived yes)
      (yes adult 38.3 60 16 cool normal normal-pink <3 depressed hypermotile none slight >11 3 normal distended-small-intestine 30 6 clear 3 lived yes)
      (yes adult 37.1 40 8 ? normal pale-cyanotic <3 intermittent-mild-pain hypomotile none none none ? decreased firm-feces-in-large-intestine 23 6.7 serosanguinous ? lived yes)
      (no young ? 100 44 warm normal normal-pink <3 intermittent-severe-pain hypermotile none ? ? ? normal ? 37 4.7 ? ? lived no)
      (yes adult 38.2 48 18 normal normal normal-pink <3 intermittent-mild-pain hypomotile moderate none >11 ? absent ? 48 74 clear 2 lived yes)
      (yes adult ? 60 48 cool reduced pale-cyanotic >=3 intermittent-severe-pain hypomotile severe ? ? ? ? ? 58 7.6 ? ? died yes)
      (no adult 37.9 88 24 normal normal bright-pink <3 depressed normal none ? ? ? absent normal 37 56 ? ? lived no)
      (no adult 38 44 12 cool normal normal-pink ? ? hypermotile slight ? ? ? normal ? 42 64 ? ? lived no)
      (no adult 38.5 60 20 normal normal bright-red >=3 depressed normal none slight none ? increased firm-feces-in-large-intestine 63 7.5 cloudy 2.3 euthanized no)
      (no adult 38.5 96 36 cool reduced ? >=3 depressed absent slight none >11 ? absent distended-large-intestine 70 8.5 ? ? died yes)
      (no adult 38.3 60 20 normal normal normal-pink >=3 alert-no-pain hypomotile none ? ? ? decreased ? 34 66 ? ? lived no)
      (no adult 38.5 60 40 cool normal bright-pink <3 depressed hypermotile slight ? ? ? decreased other 49 59 ? ? lived no)
      (yes adult 37.3 48 12 normal ? pale-pink <3 intermittent-mild-pain hypermotile moderate slight none ? decreased firm-feces-in-large-intestine 40 6.6 cloudy ? lived yes)
      (yes adult 38.5 86 ? normal normal pale-pink <3 intermittent-severe-pain absent moderate slight none ? decreased distended-large-intestine 45 7.4 clear 3.4 died yes)
      (yes adult 37.5 48 40 ? ? ? ? ? ? ? none none ? ? distended-large-intestine 41 55 serosanguinous 2 euthanized yes)
      (no adult 37.2 36 9 normal normal normal-pink <3 depressed hypomotile none slight none ? absent normal 35 5.7 ? ? lived no)
      (yes adult 39.2 ? 23 cool normal pale-pink <3 intermittent-severe-pain absent slight slight ? ? ? ? 36 6.6 clear 3 lived yes)
      (no adult 38.5 100 ? cool reduced bright-red >=3 intermittent-severe-pain hypomotile severe slight none ? absent distended-large-intestine ? ? ? ? euthanized no)
      (yes adult 38.5 96 30 warm reduced pale-cyanotic >=3 intermittent-severe-pain absent moderate slight none ? decreased distended-large-intestine 50 65 ? ? lived yes)
      (yes adult ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? 45 8.7 ? ? died yes)
      (yes adult 37.8 88 80 cool reduced bright-red >=3 ? hypomotile moderate slight <11 ? absent distended-large-intestine 64 89 ? ? euthanized yes)
      (no adult 37.5 44 10 cool normal normal-pink <3 intermittent-mild-pain hypermotile slight slight ? ? decreased firm-feces-in-large-intestine 43 51 clear 1 lived no)
      (yes adult 37.9 68 20 ? normal bright-pink <3 depressed absent slight ? ? ? normal distended-large-intestine 45 4 serosanguinous 2.8 died yes)
      (yes adult 38 86 24 cold reduced pale-cyanotic <3 depressed absent severe none none ? absent distended-large-intestine 45 5.5 clear 10.1 died yes)
      (yes young 38.9 120 30 normal reduced bright-pink >=3 intermittent-mild-pain hypomotile moderate significant none 3 ? ? 47 6.3 clear ? lived no)
      (yes adult 37.6 45 12 cool normal pale-pink <3 ? normal slight slight none ? normal distended-small-intestine 39 7 cloudy 1.5 lived yes)
      (no adult 38.6 56 32 warm normal normal-pink <3 alert-no-pain hypermotile none slight ? ? increased ? 40 7 cloudy 2.1 lived no)
      (yes adult 37.8 40 12 normal normal normal-pink <3 alert-no-pain normal none slight none ? normal other 38 7 ? ? lived yes)
      (no adult ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? lived no)
      (yes adult 38 76 18 ? ? ? >=3 ? ? ? ? ? ? ? ? 71 11 ? ? lived yes)
      (yes adult 38.1 40 36 normal increased bright-pink <3 depressed normal ? ? ? ? ? ? ? ? ? ? euthanized yes)
      (yes adult ? 52 28 cool reduced pale-cyanotic <3 intermittent-mild-pain absent moderate slight none ? absent distended-small-intestine 37 8.1 ? ? lived yes)
      (yes adult 39.2 88 58 cold absent ? >=3 continuous-severe-pain absent ? ? ? ? ? ? ? ? cloudy 2 euthanized no)
      (yes adult 38.5 92 40 cold reduced ? <3 depressed absent moderate ? ? ? absent ? 46 67 cloudy 2 lived yes)
      (yes adult ? 112 13 cold absent pale-cyanotic <3 depressed hypomotile none slight none 4.5 absent distended-small-intestine 60 6.3 serosanguinous ? lived yes)
      (yes adult 37.7 66 12 normal normal pale-pink <3 intermittent-mild-pain hypomotile slight slight ? ? absent distended-small-intestine 31.5 6.2 cloudy 1.6 lived yes)
      (yes adult 38.8 50 14 normal normal normal-pink <3 intermittent-mild-pain hypermotile none none none ? decreased distended-large-intestine 38 58 ? ? lived yes)
      (no adult 38.4 54 24 normal normal normal-pink <3 alert-no-pain hypomotile none slight none ? decreased other 49 7.2 clear 8 lived no)
      (yes adult 39.2 120 20 cold reduced bright-red >=3 depressed hypomotile moderate none <11 ? ? distended-small-intestine 60 8.8 serosanguinous ? died yes)
      (yes young ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? 45 6.5 cloudy ? lived yes)
      (yes adult 37.3 90 40 cool ? dark-cyanotic >=3 continuous-severe-pain absent moderate slight >11 ? normal distended-large-intestine 65 50 serosanguinous 2 euthanized yes)
      (yes young 38.5 120 70 ? ? ? ? ? hypermotile ? slight ? ? normal ? 35 54 clear 1 lived yes)
      (yes adult 38.5 104 40 cool reduced ? <3 intermittent-severe-pain hypomotile severe ? ? ? ? ? ? ? ? ? lived yes)
      (no adult 39.5 92 28 cool reduced dark-cyanotic <3 continuous-severe-pain absent none ? <11 ? absent ? 72 6.4 ? 3.6 died no)
      (yes adult 38.5 30 18 ? ? ? ? ? ? ? ? ? ? ? ? 40 7.7 ? ? lived yes)
      (yes adult 38.3 72 30 cold reduced pale-pink >=3 intermittent-mild-pain hypomotile moderate slight none ? decreased distended-large-intestine 43 7 cloudy 3.9 lived yes)
      (no adult 37.5 48 30 cold normal pale-pink <3 ? normal none none none ? normal normal 48 8.6 ? ? lived no)
      (yes adult 38.1 52 24 normal normal bright-red <3 intermittent-severe-pain hypomotile none slight <11 7 normal ? 54 7.5 cloudy 2.6 died yes)
      (no adult 38.2 42 26 normal normal normal-pink <3 intermittent-mild-pain hypermotile slight ? ? ? normal ? 36 6.9 ? ? lived no)
      (no adult 37.9 54 42 warm normal bright-red <3 intermittent-mild-pain hypermotile none ? none ? ? other 47 54 serosanguinous 1 lived no)
      (no adult 36.1 88 ? cool reduced pale-pink <3 intermittent-mild-pain hypomotile slight slight <11 ? ? distended-small-intestine 45 7 serosanguinous 4.8 euthanized yes)
      (yes adult 38.1 70 22 ? normal ? <3 continuous-severe-pain hypomotile ? ? ? ? ? distended-large-intestine 36 65 ? ? euthanized yes)
      (yes adult 38 90 30 cold reduced pale-cyanotic >=3 continuous-severe-pain absent severe ? ? ? absent distended-large-intestine 55 6.1 ? ? died yes)
      (yes adult 38.2 52 16 normal normal bright-pink <3 alert-no-pain normal none none none ? normal ? 43 8.1 ? ? lived no)
      (yes adult ? 36 32 normal normal pale-cyanotic <3 continuous-severe-pain hypomotile moderate slight <11 4 ? distended-small-intestine 41 5.9 ? ? died yes)
      (yes adult 38.4 92 20 normal ? ? >=3 ? hypomotile moderate ? ? ? normal ? ? ? ? ? lived yes)
      (yes young 38.2 124 88 normal reduced bright-pink <3 depressed hypomotile severe ? ? ? ? ? 47 8 clear ? lived yes)
      (no adult ? 96 ? cool reduced pale-pink >=3 continuous-severe-pain absent severe ? none ? absent distended-large-intestine 60 ? ? ? died yes)
      (yes adult 37.6 68 32 cool ? pale-pink <3 intermittent-severe-pain normal severe slight >11 6.5 normal distended-large-intestine 47 7.2 clear ? lived yes)
      (yes adult 38.1 88 24 cool reduced pale-cyanotic <3 continuous-severe-pain absent moderate slight none ? decreased distended-small-intestine 41 4.6 ? ? died yes)
      (yes adult 38 108 60 warm reduced pale-cyanotic <3 intermittent-severe-pain hypomotile moderate slight ? ? decreased distended-small-intestine ? ? serosanguinous ? lived yes)
      (no adult 38.2 48 ? warm ? normal-pink >=3 intermittent-mild-pain hypomotile none slight none ? ? other 34 6.6 ? ? lived no)
      (yes adult 39.3 100 51 cold absent dark-cyanotic <3 depressed absent none none <11 2 ? distended-small-intestine 66 13 serosanguinous 2 euthanized yes)
      (no adult 36.6 42 18 cool reduced bright-pink <3 alert-no-pain absent none none none ? ? distended-large-intestine 52 7.1 ? ? died yes)
      (yes young 38.8 124 36 cool normal bright-pink <3 depressed hypomotile severe none none ? absent distended-small-intestine 50 7.6 serosanguinous ? died yes)
      (no adult ? 112 24 cool reduced pale-cyanotic >=3 continuous-severe-pain absent slight ? ? ? absent ? 40 5.3 serosanguinous 2.6 lived no)
      (yes adult ? 80 ? cool reduced pale-pink <3 intermittent-severe-pain absent severe ? ? ? absent distended-large-intestine 43 70 ? ? lived yes)
      (yes young 38.8 184 84 normal ? normal-pink <3 intermittent-severe-pain hypermotile moderate ? ? ? increased ? 33 3.3 ? ? died yes)
      (yes adult 37.5 72 ? warm normal normal-pink <3 depressed hypermotile none none none ? normal ? 35 65 cloudy 2 euthanized yes)
      (yes adult 38.7 96 28 cool reduced pale-cyanotic <3 ? absent ? ? <11 7.5 ? ? 64 9 ? ? died yes)
      (no adult 37.5 52 12 normal normal normal-pink <3 depressed hypomotile slight slight none ? decreased distended-large-intestine 36 61 clear 1 lived no)
      (yes adult 40.8 72 42 cool reduced normal-pink <3 depressed hypomotile none slight none ? ? ? 54 7.4 serosanguinous ? died yes)
      (no adult 38 40 25 ? normal normal-pink <3 intermittent-severe-pain hypomotile slight none none ? absent ? 37 69 ? ? lived no)
      (no adult 38.4 48 16 warm normal normal-pink <3 alert-no-pain ? slight slight none ? ? other 39 6.5 ? ? lived no)
      (no young 38.6 88 28 ? ? ? ? ? ? ? ? ? ? ? ? 35 5.9 ? ? lived no)
      (yes adult 37.1 75 36 ? ? pale-pink >=3 intermittent-severe-pain absent slight slight <11 5 absent distended-small-intestine 48 7.4 serosanguinous 3.2 died yes)
      (yes adult 38.3 44 21 cool normal bright-pink <3 intermittent-mild-pain hypomotile moderate slight none ? normal distended-large-intestine 44 6.5 cloudy 4.4 lived yes)
      (no adult ? 56 68 cool normal normal-pink <3 intermittent-mild-pain hypomotile none slight none ? normal ? 40 6 ? ? euthanized yes)
      (no adult 38.6 68 20 warm normal pale-pink <3 intermittent-mild-pain hypomotile slight none none ? normal distended-large-intestine 38 6.5 clear ? lived no)
      (no adult 38.3 54 18 cool normal bright-pink <3 depressed hypomotile slight ? <11 5.4 ? distended-small-intestine 44 7.2 serosanguinous ? lived no)
      (yes adult 38.2 42 20 ? ? normal-pink <3 ? hypomotile ? ? ? ? decreased ? 47 60 ? ? lived no)
      (yes adult 39.3 64 90 warm reduced normal-pink <3 ? hypomotile none none >11 6.5 normal distended-large-intestine 39 6.7 ? ? lived yes)
      (yes adult 37.5 60 50 cool reduced normal-pink <3 intermittent-mild-pain hypomotile slight slight >11 3.5 decreased distended-small-intestine 35 6.5 ? ? died yes)
      (yes adult 37.7 80 ? cool reduced dark-cyanotic <3 continuous-severe-pain absent none slight <11 ? decreased normal 50 55 serosanguinous 2 lived yes)
      (yes adult ? 100 30 cool reduced pale-cyanotic >=3 continuous-severe-pain absent severe significant <11 ? absent distended-small-intestine 52 6.6 ? ? lived yes)
      (yes adult 37.7 120 28 cool reduced pale-pink <3 continuous-severe-pain hypomotile moderate none none ? ? ? 65 7 serosanguinous ? died yes)
      (yes adult ? 76 ? ? reduced ? ? ? absent severe ? ? ? ? distended-large-intestine ? ? ? ? euthanized yes)
      (yes young 38.8 150 50 normal reduced dark-cyanotic >=3 continuous-severe-pain hypomotile slight none none ? ? ? 50 6.2 ? ? died yes)
      (yes adult 38 36 16 cool normal normal-pink <3 intermittent-severe-pain normal slight significant <11 2 decreased ? 37 75 cloudy 1 euthanized no)
      (no adult 36.9 50 40 warm reduced pale-pink <3 alert-no-pain hypomotile slight significant none 7 ? ? 37.5 6.5 ? ? lived no)
      (no adult 37.8 40 16 normal normal normal-pink <3 alert-no-pain hypermotile none ? ? ? normal normal 37 6.8 ? ? lived no)
      (no adult 38.2 56 40 cold reduced normal-pink <3 depressed absent moderate slight >11 7.5 ? ? 47 7.2 clear 2.5 lived no)
      (yes adult 38.6 48 12 ? ? normal-pink ? alert-no-pain hypermotile ? ? ? ? ? ? 36 67 ? ? lived no)
      (no adult 40 78 ? cool reduced bright-red <3 depressed hypomotile none none none ? absent normal 66 6.5 ? ? died yes)
      (yes adult ? 70 16 cool absent bright-red >=3 depressed hypomotile slight slight none ? absent distended-large-intestine 60 7.5 ? ? died yes)
      (yes adult 38.2 72 18 ? ? ? ? ? ? ? ? ? ? ? ? 35 6.4 ? ? lived yes)
      (no adult 38.5 54 ? normal normal normal-pink <3 intermittent-mild-pain hypermotile none slight none ? normal ? 40 6.8 cloudy 7 lived no)
      (yes adult 38.5 66 24 normal normal normal-pink <3 intermittent-mild-pain hypomotile none slight none ? absent distended-large-intestine 40 6.7 clear ? lived yes)
      (no adult 37.8 82 12 cool normal normal-pink >=3 intermittent-severe-pain ? moderate none <11 ? ? ? 50 7 ? ? euthanized yes)
      (no young 39.5 84 30 ? ? ? <3 ? ? ? ? ? ? ? ? 28 5 ? ? lived no)
      (yes adult ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? lived yes)
      (yes adult 38 50 36 ? normal normal-pink <3 intermittent-mild-pain normal slight ? ? ? decreased ? 39 6.6 clear 5.3 lived yes)
      (no adult 38.6 45 16 warm normal bright-pink <3 alert-no-pain hypermotile ? ? ? ? normal normal 43 58 ? ? lived no)
      (yes adult 38.9 80 44 cool reduced pale-pink <3 depressed hypomotile moderate slight >11 7 decreased normal 54 6.5 serosanguinous ? died yes)
      (yes adult 37 66 20 normal reduced bright-pink <3 intermittent-severe-pain hypomotile moderate none ? ? normal distended-large-intestine 35 6.9 cloudy ? died yes)
      (yes adult ? 78 24 cool reduced pale-pink <3 ? hypomotile ? slight none ? ? distended-small-intestine 43 62 ? 2 euthanized no)
      (no adult 38.5 40 16 normal normal normal-pink <3 depressed hypermotile none ? ? ? decreased other 37 67 ? ? lived no)
      (yes adult ? 120 70 cold ? pale-cyanotic >=3 depressed absent ? ? ? ? ? distended-large-intestine 55 65 ? ? euthanized no)
      (no adult 37.2 72 24 cool increased pale-cyanotic >=3 intermittent-severe-pain hypomotile moderate significant none ? absent distended-small-intestine 44 ? serosanguinous 3.3 euthanized yes)
      (yes adult 37.5 72 30 cold reduced pale-cyanotic <3 intermittent-severe-pain absent moderate slight none ? decreased distended-large-intestine 60 6.8 ? ? died yes)
      (yes adult 36.5 100 24 cool reduced pale-pink <3 intermittent-mild-pain hypomotile moderate significant none ? absent distended-small-intestine 50 6 serosanguinous 3.4 lived yes)
      (yes adult 37.2 40 20 ? ? ? ? ? ? ? ? ? ? absent normal 36 62 clear 1 euthanized no)
      (no adult 38.5 54 20 ? normal bright-pink >=3 intermittent-mild-pain absent none slight >11 5.9 ? other 42 6.3 ? ? lived no)
      (no adult 37.6 48 36 ? ? normal-pink <3 ? hypomotile ? ? ? ? ? ? 44 6.3 clear 5 lived no)
      (yes adult 37.7 44 28 ? absent pale-pink >=3 continuous-severe-pain absent severe none none ? decreased distended-large-intestine 45 70 serosanguinous 2 lived yes)
      (yes adult 37 56 24 cool normal pale-cyanotic >=3 intermittent-severe-pain absent moderate none none ? ? ? 35 61 serosanguinous 2 euthanized no)
      (no adult 38 42 12 cool ? pale-pink <3 alert-no-pain ? none ? ? ? ? other 37 5.8 ? ? lived no)
      (yes adult ? 60 40 cool ? normal-pink <3 ? absent ? significant >11 ? ? distended-large-intestine 42 72 ? ? lived yes)
      (no adult 38.4 80 60 cool increased bright-pink <3 intermittent-mild-pain normal none slight >11 ? normal normal 54 6.9 ? ? lived no)
      (no adult 37.8 48 12 warm normal bright-pink <3 intermittent-mild-pain ? none slight ? ? increased ? 48 7.3 clear ? lived no)
      (? adult 38 65 40 ? normal pale-cyanotic >=3 depressed ? ? ? ? ? ? distended-large-intestine ? ? ? ? ? yes)
      (no adult 37.9 45 36 cool reduced pale-pink >=3 depressed hypomotile none slight none ? decreased ? 33 5.7 serosanguinous ? lived yes)
      (no adult 39 84 12 cool normal bright-red <3 depressed absent slight none >11 7 ? distended-small-intestine 62 5.9 cloudy 2.2 died yes)
      (no adult 38.2 60 24 cool normal pale-pink >=3 intermittent-mild-pain hypomotile slight significant <11 ? absent distended-small-intestine 53 7.5 cloudy 1.4 lived no)
      (yes adult ? 140 ? ? ? pale-cyanotic >=3 continuous-severe-pain absent severe none none ? ? distended-large-intestine 30 69 ? ? died no)
      (yes adult 37.9 120 60 cool reduced pale-pink <3 continuous-severe-pain absent severe slight >11 7.5 absent distended-large-intestine 52 6.6 serosanguinous 1.8 died yes)
      (no adult 38 72 36 normal normal pale-pink <3 intermittent-mild-pain ? slight slight none ? decreased distended-large-intestine 38 6.8 cloudy 2 lived no)
      (no young 38 92 28 normal normal bright-pink <3 alert-no-pain hypomotile slight significant ? 7.2 ? ? 37 6.1 clear 1.1 lived no)
      (yes adult 38.3 66 30 warm reduced normal-pink <3 depressed absent moderate significant >11 8.5 absent distended-large-intestine 37 6 ? ? lived yes)
      (no adult 37.5 48 24 cool normal normal-pink <3 depressed hypermotile ? none none ? decreased other 43 6 clear 2.8 lived no)
      (yes adult 37.5 88 20 warm reduced pale-pink <3 intermittent-severe-pain hypomotile moderate ? ? ? ? ? 35 6.4 clear ? died yes)
      (no young ? 150 60 cold absent pale-cyanotic >=3 continuous-severe-pain absent severe ? ? ? ? ? ? ? ? ? died yes)
      (yes adult 39.7 100 30 ? ? dark-cyanotic >=3 intermittent-severe-pain absent moderate none ? ? absent distended-large-intestine 65 75 ? ? euthanized yes)
      (yes adult 38.3 80 ? cool reduced pale-cyanotic >=3 continuous-severe-pain absent moderate slight none ? absent distended-small-intestine 45 7.5 cloudy 4.6 lived yes)
      (no adult 37.5 40 32 cool normal pale-pink <3 intermittent-mild-pain normal moderate slight none ? ? distended-large-intestine 32 6.4 clear 1.1 lived yes)
      (yes adult 38.4 84 30 cool normal bright-red >=3 intermittent-severe-pain hypomotile moderate slight <11 6.5 absent distended-small-intestine 47 7.5 serosanguinous ? died yes)
      (yes adult 38.1 84 44 cold ? pale-cyanotic >=3 continuous-severe-pain hypomotile none none <11 5 ? distended-small-intestine 60 6.8 ? 5.7 died yes)
      (no adult 38.7 52 ? normal normal normal-pink <3 alert-no-pain hypomotile none ? ? ? normal firm-feces-in-large-intestine 4 74 ? ? lived no)
      (no adult 38.1 44 40 warm normal pale-pink <3 intermittent-mild-pain hypomotile none ? ? ? normal firm-feces-in-large-intestine 35 6.8 ? ? lived no)
      (no adult 38.4 52 20 warm normal pale-pink <3 alert-no-pain hypomotile slight slight none ? decreased distended-large-intestine 41 63 clear 1 lived no)
      (yes adult 38.2 60 ? normal ? pale-pink <3 depressed hypermotile none none none ? absent distended-small-intestine 43 6.2 cloudy 3.9 lived yes)
      (no adult 37.7 40 18 normal normal normal-pink ? intermittent-mild-pain normal none none none ? decreased firm-feces-in-large-intestine 36 3.5 ? ? lived no)
      (yes adult 39.1 60 10 ? normal normal-pink ? depressed hypomotile ? ? ? ? absent distended-small-intestine ? ? ? ? lived yes)
      (no adult 37.8 48 16 normal normal normal-pink <3 ? hypermotile none slight none ? absent firm-feces-in-large-intestine 43 7.5 ? ? lived no)
      (yes adult 39 120 ? cold reduced bright-red >=3 depressed absent moderate slight <11 8 ? ? 65 8.2 serosanguinous 4.6 lived no)
      (yes adult 38.2 76 ? warm reduced bright-pink <3 continuous-severe-pain hypomotile moderate none >11 6 normal distended-large-intestine 35 6.5 cloudy 0.9 lived yes)
      (no adult 38.3 88 ? ? ? dark-cyanotic ? ? ? ? ? ? ? ? ? ? ? ? ? died no)
      (yes adult 38 80 30 cool reduced pale-pink <3 ? ? ? ? ? 6 ? ? 48 8.3 ? 4.3 lived yes)
      (yes adult ? ? ? cool normal normal-pink <3 depressed hypomotile moderate none <11 6 absent distended-small-intestine ? ? cloudy ? died yes)
      (yes adult 37.6 40 ? normal normal normal-pink <3 alert-no-pain hypermotile none ? ? ? normal normal ? ? cloudy 2.1 lived yes)
      (no adult 37.5 44 ? normal normal normal-pink <3 intermittent-mild-pain hypomotile slight ? ? ? ? ? 45 5.8 cloudy 1.4 lived no)
      (no adult 38.2 42 16 normal normal pale-pink <3 alert-no-pain hypomotile none ? ? ? normal ? 35 60 clear 1 lived no)
      (no adult 38 56 44 cool reduced pale-pink ? ? hypermotile none slight none ? absent ? 47 70 cloudy 1 lived no)
      (no adult 38.3 45 20 cool reduced bright-pink >=3 depressed absent none slight ? ? absent ? ? ? ? ? lived no)
      (yes adult ? 48 96 normal normal pale-pink <3 ? absent none slight none ? normal distended-small-intestine 42 8 clear ? lived yes)
      (yes adult 37.7 55 28 warm normal bright-pink <3 depressed hypomotile moderate ? <11 5 absent distended-large-intestine ? ? ? ? lived yes)
      (no adult 36 100 20 cold reduced dark-cyanotic >=3 depressed absent moderate none none ? absent distended-large-intestine 74 5.7 cloudy 2.5 euthanized yes)
      (yes adult 37.1 60 20 warm ? pale-cyanotic <3 intermittent-mild-pain ? moderate ? >11 5 decreased distended-small-intestine 64 8.5 cloudy ? lived yes)
      (no adult 37.1 114 40 cool ? pale-pink >=3 depressed normal none ? ? ? ? firm-feces-in-large-intestine 32 ? serosanguinous 6.5 lived no)
      (yes adult 38.1 72 30 cool reduced pale-pink <3 intermittent-severe-pain absent moderate slight none ? decreased distended-large-intestine 37 56 serosanguinous 1 lived yes)
      (yes adult 37 44 12 cool normal normal-pink >=3 alert-no-pain hypermotile none ? ? ? absent other 40 6.7 serosanguinous 8 lived yes)
      (yes adult 38.6 48 20 cool normal normal-pink <3 intermittent-severe-pain hypomotile none ? ? ? decreased ? 37 75 ? ? lived yes)
      (yes adult ? 82 72 cool normal pale-cyanotic <3 depressed hypomotile moderate ? <11 ? absent distended-small-intestine 53 65 serosanguinous 2 euthanized yes)
      (yes young 38.2 78 60 cold absent dark-cyanotic ? intermittent-mild-pain hypomotile moderate ? ? ? normal ? 59 5.8 serosanguinous 3.1 died yes)
      (no adult 37.8 60 16 normal normal pale-pink <3 depressed hypomotile slight none >11 ? decreased ? 41 73 ? ? euthanized no)
      (yes adult 38.7 34 30 warm ? pale-pink <3 depressed hypomotile ? ? ? ? ? ? 33 69 ? 2 euthanized yes)
      (yes adult ? 36 12 normal normal normal-pink <3 alert-no-pain normal none none none ? normal distended-large-intestine 44 ? ? ? lived yes)
      (no adult 38.3 44 60 ? ? normal-pink <3 ? ? ? ? ? ? ? ? 6.4 36 ? ? lived yes)
      (no adult 37.4 54 18 cool ? normal-pink <3 intermittent-mild-pain absent moderate slight >11 ? absent distended-large-intestine 30 7.1 cloudy ? lived yes)
      (yes adult ? ? ? cold reduced ? >=3 depressed absent none ? ? ? ? ? 54 76 serosanguinous 2 lived yes)
      (yes adult 36.6 48 16 cool normal pale-pink <3 intermittent-severe-pain hypermotile none none none ? ? ? 27 56 ? ? euthanized yes)
      (yes adult 38.5 90 ? normal normal pale-pink <3 intermittent-mild-pain hypomotile moderate slight <11 2 absent distended-large-intestine 47 79 ? ? lived yes)
      (yes adult ? 75 12 normal normal pale-cyanotic <3 continuous-severe-pain hypomotile moderate ? <11 5.8 ? ? 58 8.5 clear ? lived yes)
      (no adult 38.2 42 ? cool normal normal-pink <3 alert-no-pain hypermotile slight slight none ? decreased other 35 5.9 cloudy ? lived no)
      (yes young 38.2 78 60 cold absent dark-cyanotic ? intermittent-mild-pain hypomotile moderate ? ? ? normal ? 59 5.8 serosanguinous 3.1 died yes)
      (no adult 38.6 60 30 normal normal pale-pink <3 intermittent-severe-pain normal slight none none ? ? ? 40 6 clear ? lived yes)
      (no adult 37.8 42 40 normal normal normal-pink <3 alert-no-pain hypomotile none ? ? ? decreased firm-feces-in-large-intestine 36 6.2 ? ? lived no)
      (yes adult 38 60 12 normal normal bright-pink <3 depressed hypermotile none none none ? normal distended-small-intestine 44 65 serosanguinous 2 euthanized yes)
      (no adult 38 42 12 cool ? pale-pink <3 alert-no-pain hypermotile none ? ? ? ? normal 37 5.8 ? ? lived no)
      (no adult 37.6 88 36 cool normal normal-pink <3 intermittent-mild-pain hypomotile slight none <11 1.5 ? ? 44 6 ? ? died yes)
      )))
