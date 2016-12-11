(ns adventure.core
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str])
  (:gen-class))

(def engineering-campus {
        :SC-atrium {
                :desc ""
                :title "in the atrium of the Thomas M. Siebel Center for Computer Science"
                :dir {:north :SC-1104, :south :SC-1210, :east :SC-1404, :west :first-floor-elevator}
                :courses {}
                :items [:coffee]}
        :basement-elevator {
                :desc ""
                :title "by the basement elevator"
                :dir {:up :first-floor-elevator, :east :SC-0216}
                :courses {}
                :items []}
        :first-floor-elevator {
                :desc ""
                :title "by the first floor elevator"
                :dir {:down :basement-elevator, :up :second-floor-elevator, :west :DCL-1320, :east :SC-atrium}
                :courses {}
                :items []}
        :second-floor-elevator {
                :desc ""
                :title "by the second floor elevator"
                :dir {:down :first-floor-elevator, :up :third-floor-elevator, :north :SC-2124}
                :courses {}
                :items []}
        :third-floor-elevator {
                :desc ""
                :title "by the third floor elevator"
                :dir {:down :second-floor-elevator, :up :fourth-floor-elevator}
                :courses {}
                :items []}
        :fourth-floor-elevator {
                :desc ""
                :title "by the fourth floor elevator"
                :dir {:down :third-floor-elevator, :north :SC-4107}
                :courses {}
                :items []}
        :DCL-1320 {
                :desc ""
                :title "in the Digital Computer Laboratory lecture hall 1320"
                :dir {:south :Grainger, :east :first-floor-elevator, :west :ECEB-1002}
                :courses {:freshman :CS125, :sophomore :CS233, :senior :CS411}
                :items [:pen]}
        :SC-1105 {
                :desc ""
                :title "in Siebel Center room 1105"
                :dir {:south :SC-1104}
                :courses {:freshman :CS126}
                :items []}
        :ECEB-1002 {
                :desc ""
                :title "in the Electrical and Computer Engineering Building lecture hall 1002"
                :dir {:east :DCL-1320}
                :courses {:freshman :CS173, :sophomore :CS225, :junior :CS374}
                :items [:TI-84]}
        :SC-0218 {
                :desc ""
                :title "in the Siebel Center lab 0218"
                :dir {:west :SC-0216}
                :courses {:sophomore :CS241}
                :items []}
        :SC-0216 {
                :desc ""
                :title "in the Siebel Center lab 0216"
                :dir {:east :SC-0218, :west :basement-elevator}
                :courses {:sophomore :CS296-25, :senior :CS440}
                :items []}
        :SC-1404 {
                :desc ""
                :title "in the Siebel Center lecture hall 1404"
                :dir {:west :SC-atrium}
                :courses {:junior :CS357, :senior :CS421}
                :items [:swag]}
        :Grainger {
                :desc ""
                :title "in the Grainger Engineering Library"
                :dir {:north :DCL-1320}
                :courses {:junior :CS242}
                :items [:markers]}
        :SC-1304 {
                :desc ""
                :title "in Siebel Center room 1304"
                :dir {:east :SC-1210}
                :courses {:senior :CS210}
                :items []}
        :SC-4107 {
                :desc "the virtual reality lab"
                :title "in the Siebel Center lab 4107"
                :dir {:south :fourth-floor-elevator}
                :courses {:senior :CS498}
                :items [:oculus]}
        :SC-1210 {
                :desc "the academic advising office"
                :title "in the Siebel Center office 1210"
                :dir {:north :SC-atrium, :east :SC-1318, :west :SC-1304}
                :courses {}
                :items [:lollipop]}
        :SC-1318 {
                :desc "the Women in Computer Science (WCS) office"
                :title "in the Siebel Center office 1318"
                :dir {:east :SC-1320, :west :SC-1210}
                :courses {}
                :items [:pancake]}
        :SC-1104 {
                :desc "the Association for Computing Machinery (ACM) office"
                :title "in the Siebel Center office 1104"
                :dir {:north :SC-1105, :south :SC-atrium}
                :courses {}
                :items [:pizza]}
        :SC-1320 {
                :desc "the Latinos in Computer Science (LCS) office"
                :title "in the Siebel Center office 1320"
                :dir {:west :SC-1318}
                :courses {}
                :items []}
        :SC-2124 {
                :desc "the CocoaNuts meeting room"
                :title "in the Siebel Center conference room 2124"
                :dir {:south :second-floor-elevator}
                :courses {}
                :items [:friends]}
   })

(def adventurer {
          :location :SC-atrium
          :classrank :freshman
          :credits []
          :moves-remaining-in-semester 1000
          :backpack []})

(defn status [player map]
        (println (str "You are a " (name (player :classrank)) ". "))
        (let [room (get map (player :location))]
                (print (str "You are " (get room :title) ". "))))

(defn to-keywords [commands]
  (mapv keyword (str/split commands #"[.,?! ]+")))

(defn go [dir player map]
  (let [location (player :location)
        dest (->> map location :dir dir)]
    (if (nil? dest)
        (do (println "You can't go that way.")
              [player map])
        [(assoc-in player [:location] dest) map])))

 (defn look [player map]
   (let [location (player :location)
        classrank (player :classrank)
        room (get map location)
        courses-offered (get room :courses)
        items (get room :items)]

        (println "These are the courses offered" (get room :title) ":")
        (println courses-offered)

        (if (> (count items) 0)
                (do (println "\nOMG, you discovered: ")
                    (println items))))
        [player map])

(defn take [player map]
        (let [location (player :location)
             room (get map location)
             items (get room :items)]

             (if (> (count items) 0)
                (do (def newPlayer (assoc-in player [:backpack] (conj (player :backpack) (peek items))))
                        (def newItems (pop items))
                        (def newRoom (assoc-in room [:items] newItems))
                        (def newMap (assoc-in map [location] newRoom))
                        [newPlayer newMap])
                (do (println "There are no items here to take!  Use the look command to view the contents of a room.")
                     [player map]))))

(defn drop [player map]
        [player map]
        )

(defn backpack [player map]
        [player, map]
        )

 (defn DARS [player map]
         (println "")
         (println "")
         (println "---------------------------------")
         (println "REQUIREMENTS")
         (println "     Freshman: CS125, CS126, CS173")
         (println "     Sophomore: CS225, CS233, CS241, CS296-25")
         (println "     Junior: CS242, CS374, CS357")
         (println "     Senior: CS210, CS411, CS421, CS440, CS498")
         (println "---------------------------------")
         (println "")
         (println "YOUR CREDITS:")
         (println (player :credits))
         (println "")
         [player map])

 (defn learn [player map]
   (let [location (player :location)
        classrank (player :classrank)
        room (get map location)
        courses-offered (get room :courses)
        course (get courses-offered classrank)]

     (if (nil? course)
        (do (println "There are no classes offered for a" (name classrank) "in this room.")
                [player map])
        (do
                (if (contains? (set (player :credits)) course)
                        (do (println "You have already taken all courses available for a" (name classrank) "in" (name location)"!  Come back later or check your DARS report for the requirements to advance to the next year.")
                                [player map])
                        (do (def newPlayer (assoc-in player [:credits] (conj (player :credits) course)))
                             (println "Congratulations, you've earned credit for" (name course) "!")

                             ;   REQUIREMENTS
                             ;     Freshman: CS125, CS126, CS173
                             ;     Sophomore: CS225, CS233, CS241, and CS296-25
                             ;     Junior: CS242, CS374, CS357
                             ;     Senior: CS210, CS411, CS421, CS440, CS498
                                 (cond
                                         (and (= (newPlayer :classrank) :freshman)
                                              (clojure.set/subset? (set '(:CS125, :CS126, :CS173)) (set (newPlayer :credits))))
                                                     (assoc-in newPlayer [:classrank] :sophomore)

                                         (and (= (newPlayer :classrank) :sophomore)
                                              (clojure.set/subset? (set '(:CS225, :CS233, :CS241, :CS296-25)) (set (newPlayer :credits))))
                                                     (assoc-in newPlayer [:classrank] :junior)

                                         (and (= (newPlayer :classrank) :junior)
                                              (clojure.set/subset? (set '(:CS242, :CS374, :CS357)) (set (newPlayer :credits))))
                                                     (assoc-in newPlayer [:classrank] :senior)

                                         (clojure.set/subset? (set '(:CS210, :CS411, :CS421, :CS440, :CS498)) (set (newPlayer :credits)))
                                         (do (println "Congratulations, you have made it through hell!")
                                              [newPlayer map])

                                         :else [newPlayer map])))))))

(defn respond [player map command]
  (match command
          [:north] (go :north player map)
          [:n] (go :north player map)
          [:south] (go :south player map)
          [:s] (go :south player map)
          [:east] (go :east player map)
          [:e] (go :east player map)
          [:west] (go :west player map)
          [:w] (go :west player map)
          [:down] (go :down player map)
          [:d] (go :down player map)
          [:up] (go :up player map)
          [:u] (go :up player map)

          [:learn] (learn player map)
          [:DARS] (DARS player map)
          [:look] (look player map)

          [:take] (take player map)
          [:drop] (drop player map)
          [:backpack] (backpack player map)

         _ (do (println "I don't understand you.")
               [player map])

         ))

(defn -main [& args]
   (println "\n\n---------- CS @ Illinois: The Text Adventure ----------\n\n")
   (println "Welcome to the University of Illinois at Urbana-Champaign!\nYou are a freshman in one of the most prestigious CS programs in the world.\nOver the next four years, you will navigate forests of up-trees, seas of segfaults, and maelstroms of multiplexors.\nIt will take all your wits to survive.\n\nYour goal is to obtain the necessary prerequisites to advance your class rank. To win, you must graduate in 4 years!")
   (println "\n\n---------- may the finals be ever in your favor ----------\n\n")

  (loop [local-player adventurer
         local-map engineering-campus]
    (let [_ (println "\n****************************************")
          pl (status local-player local-map)
          _ (println "What do you want to do?")
          command (read-line)]
          (def newArgs (respond local-player local-map (to-keywords command)))
          (recur (get newArgs 0) (get newArgs 1)))))
