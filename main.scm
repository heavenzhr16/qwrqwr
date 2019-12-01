
(require csc151)

(define pokedex (map (section take <> 12)(drop (read-csv-file "/Users/havinlim/Desktop/Study/CSC-151-01/csproject/pokedex.csv") 1)))
(define pokemoves (map (section take <> 7) (drop (read-csv-file "/Users/havinlim/Desktop/Study/CSC-151-01/csproject/pokemoves.csv") 1)))

;;; Finds where the pokemon from the standard is, and stores that list
;;; Finds the type, hp, atk, def, sp.atk, sp.def, spd, and moves and returns all of the information in a vector

;;; Creates a table containing all of the moves from a given standard input 
(define get-moves
 (lambda (lst)
   (let kernel ([pos 1]
                [lst2 null])
     (if (= pos 5) lst2 (kernel (+ pos 1) (append (list (assoc (list-ref lst pos) pokemoves)) lst2))))))

(define create-pokemon
  (lambda (lst)
  (let* ([pokemon_data (assoc (car lst) (map cdr pokedex))]
         [name (cadr pokemon_data)]
         [type1 (list (drop (take pokemon_data 4) 2))]
         [type (if (number? (cadr (car type1))) (take (car type1) 1) type1)]
         [hp (list-ref pokemon_data 4)]
         [atk (list-ref pokemon_data 5)]
         [def (list-ref pokemon_data 6)]
         [sp.atk (list-ref pokemon_data 7)]
         [sp.def (list-ref pokemon_data 8)]
         [spd (list-ref pokemon_data 9)]
         [moves (get-moves lst)]
         [vec (list->vector (list name type hp atk def sp.atk sp.def spd moves))])
  vec)))

(define type-table  (list '(1 1 1 1 1 1 1 1 1 1 1 1 1/2 0 1 1 1/2 1)
                          '(1 1/2 1/2 1 2 2 1 1 1 1 1 2 1/2 1 1/2 1 2 1)
                          '(1 2 1/2 1 1/2 1 1 1 2 1 1 1 2 1 1/2 1 1 1)
                          '(1 1 2 1/2 1/2 1 1 1 0 2 1 1 1 1 1/2 1 1 1)
                          '(1 1/2 2 1 1/2 1 1 1/2 2 1/2 1 1/2 2 1 1/2 1 1/2 1)
                          '(1 1/2 1/2 1 2 1/2 1 1 2 2 1 1 1 1 2 1 1/2 1)
                          '(2 1 1 1 1 2 1 1/2 1 1/2 1/2 1/2 2 0 1 2 2 1/2)
                          '(1 1 1 1 2 1 1 1/2 1/2 1 1 1 1/2 1/2 1 1 0 2)
                          '(1 2 1 2 1/2 1 1 2 1 0 1 1/2 2 1 1 1 2 1)
                          '(1 1 1 1/2 2 1 2 1 1 1 1 2 1/2 1 1 1 1/2 1)
                          '(1 1 1 1 1 1 2 2 1 1 1/2 1 1 1 1 0 1/2 1)
                          '(1 1/2 1 1 2 1 1/2 1/2 1 1/2 2 1 1 1/2 1 2 1/2 1/2)
                          '(1 2 1 1 1 2 1/2 1 1/2 2 1 2 1 1 1 1 1/2 1)
                          '(0 1 1 1 1 1 1 1 1 1 2 1 1 2 1 1/2 1 1)
                          '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1/2 0)
                          '(1 1 1 1 1 1 1/2 1 1 1 2 1 1 2 1 1/2 1 1/2)
                          '(1 1/2 1/2 1/2 1 2 1 1 1 1 1 1 2 1 1 1 1/2 2)
                          '(1 1/2 1 1 1 1 2 1/2 1 1 1 1 1 1 2 2 1/2 1)))
(define type-pos '("Normal" "Fire" "Water" "Electric" "Grass" "Ice" "Fighting" "Poison" "Ground" "Flying" "Psychic" "Bug" "Rock"
                            "Ghost" "Dragon" "Dark" "Steel" "Fairy"))

(define overall-mod
  (lambda (atk-type def-type1 def-type2)
    (if (string-ci=? "" def-type2)
        (list-ref (list-ref type-table (index-of atk-type type-pos)) (index-of def-type1 type-pos))
        (* (list-ref (list-ref type-table (index-of atk-type type-pos)) (index-of def-type1 type-pos))
           (list-ref (list-ref type-table (index-of atk-type type-pos)) (index-of def-type2 type-pos))))))

;;; Checks if the move's type and the pokemon's type are equal.
;;; Gives 1.5 if they are equal, gives 1 if they are not equal.
(define check-move-type
  (lambda (pokemon move)
    (let* ([poke2 (assoc pokemon (map cdr pokedex))]
           [move12 (assoc move (map cdr pokemoves))])
      (cond
        [(or (equal? (cadr poke2) (cadr move12)) (equal? (caddr poke2) (cadr move12)))
         1.5]
        [else
         1]))))

;;; Calculates the damage of a move by pokemon.
;;; Pokemon2 is the defender.
(define damage-cal
  (lambda (pokemon pokemon2 move)
    (let* ([poke (assoc pokemon (map cdr pokedex))]
           [poke-opp (assoc pokemon2 (map cdr pokedex))]
           [move1 (assoc move (map cdr pokemoves))])
      (if (equal? (caddr move1) "Physical")
          (truncate (* (* (* (+ (/ (/ (* (* (+ (/ (* 50 2) 5) 2) (list-ref poke 5)) (list-ref move1 4)) (list-ref poke-opp 6)) 50) 2)
                             (check-move-type pokemon move)) (overall-mod (cadr move1) (cadr poke-opp) (caddr poke-opp))) (/ (+ 216 (random 39)) 255)))
          (truncate (* (* (* (+ (/ (/ (* (* (+ (/ (* 50 2) 5) 2) (list-ref poke 7)) (list-ref move1 4)) (list-ref poke-opp 8)) 50) 2)
                             (check-move-type pokemon move)) (overall-mod (cadr move1) (cadr poke-opp) (caddr poke-opp))) (/ (+ 216 (random 39)) 255)))))))
      

;;here is the type modifier
;;(overall-mod "Fire" "Ice" "Grass")
;4
;;it will work something like this

;; Priority
#|
+4 Detect, Endure, King's Shield, Magic Coat, Protect, Spiky Shield, Snatch
+3 Crafty Shield, Fake Out, Quick Guard, Wide Guard,
+2 Extreme Speed, Feint
+1 Aqua Jet, Baby-Doll Eyes, Bide, Bullet Punch, Ice Shard, Mach Punch, Powder, Quick Attack, Shadow Sneak, Sucker Punch, Vaccum Wave, Water Shuriken
0 Every other moves
-1 Vital Throw
-2 None
-3 Focus Punch
-4 Avalanche, Revenge
-5 Counter, Mirror Coat
-6 Circle Throw, Dragon Tail, Roar, Whirlwind
-7 Trick Room
|#

#|
607 Hold Hands
606 Celebrate
604 Electric Terrain
603 Happy Hour
602 Magnetic Flux
615 Thousand Waves
614 Thousand Arrows
270 Helping Hand
266 Follow Me
502 Ally Switch
476 Rage Powder
569 Ion Deluge

|#