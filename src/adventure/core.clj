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
                :items #{}}
        :basement-elevator {
                :desc ""
                :title "by the basement elevator"
                :dir {:up :first-floor-elevator, :east :SC-0216}
                :courses {}
                :items #{}}
        :first-floor-elevator {
                :desc ""
                :title "by the first floor elevator"
                :dir {:down :basement-elevator, :up :second-floor-elevator, :west :DCL-1320, :east :SC-atrium}
                :courses {}
                :items #{}}
        :second-floor-elevator {
                :desc ""
                :title "by the second floor elevator"
                :dir {:down :first-floor-elevator, :up :third-floor-elevator, :north :SC-2124}
                :courses {}
                :items #{}}
        :third-floor-elevator {
                :desc ""
                :title "by the third floor elevator"
                :dir {:down :second-floor-elevator, :up :fourth-floor-elevator}
                :courses {}
                :items #{}}
        :fourth-floor-elevator {
                :desc ""
                :title "by the fourth floor elevator"
                :dir {:down :third-floor-elevator, :north :SC-4107}
                :courses {}
                :items #{}}
        :DCL-1320 {
                :desc ""
                :title "in the Digital Computer Laboratory lecture hall 1320"
                :dir {:south :Grainger, :east :first-floor-elevator, :west :ECEB-1002}
                :courses {:freshman :CS125, :sophomore :CS233, :senior :CS411}
                :items #{}}
        :SC-1105 {
                :desc ""
                :title "in Siebel Center room 1105"
                :dir {:south :SC-1104}
                :courses {:freshman :CS126}
                :items #{}}
        :ECEB-1002 {
                :desc ""
                :title "in the Electrical and Computer Engineering Building lecture hall 1002"
                :dir {:east :DCL-1320}
                :courses {:freshman :CS173, :sophomore :CS225, :junior :CS374}
                :items #{}}
        :SC-0218 {
                :desc ""
                :title "in the Siebel Center lab 0218"
                :dir {:west :SC-0216}
                :courses {:sophomore :CS241}
                :items #{}}
        :SC-0216 {
                :desc ""
                :title "in the Siebel Center lab 0216"
                :dir {:east :SC-0218, :west :basement-elevator}
                :courses {:sophomore :CS296-25, :senior :CS440}
                :items #{}}
        :SC-1404 {
                :desc ""
                :title "in the Siebel Center lecture hall 1404"
                :dir {:west :SC-atrium}
                :courses {:junior :CS357, :senior :CS421}
                :items #{}}
        :Grainger {
                :desc ""
                :title "in the Grainger Engineering Library"
                :dir {:north :DCL-1320}
                :courses {:junior :CS242}
                :items #{}}
        :SC-1304 {
                :desc ""
                :title "in Siebel Center room 1304"
                :dir {:east :SC-1210}
                :courses {:senior :CS210}
                :items #{}}
        :SC-4107 {
                :desc "the virtual reality lab"
                :title "in the Siebel Center lab 4107"
                :dir {:south :fourth-floor-elevator}
                :courses {:senior :CS498}
                :items #{}}
        :SC-1210 {
                :desc "the academic advising office"
                :title "in the Siebel Center office 1210"
                :dir {:north :SC-atrium, :east :SC-1318, :west :SC-1304}
                :courses {}
                :items #{}}
        :SC-1318 {
                :desc "the Women in Computer Science (WCS) office"
                :title "in the Siebel Center office 1318"
                :dir {:east :SC-1320, :west :SC-1210}
                :courses {}
                :items #{:pancakes}}
        :SC-1104 {
                :desc "the Association for Computing Machinery (ACM) office"
                :title "in the Siebel Center office 1104"
                :dir {:north :SC-1105, :south :SC-atrium}
                :courses {}
                :items #{:pizza}}
        :SC-1320 {
                :desc "the Latinos in Computer Science (LCS) office"
                :title "in the Siebel Center office 1320"
                :dir {:west :SC-1318}
                :courses {}
                :items #{}}
        :SC-2124 {
                :desc "the CocoaNuts meeting room"
                :title "in the Siebel Center conference room 2124"
                :dir {:south :second-floor-elevator}
                :courses {}
                :items #{:friends}}
   })

(def adventurer {
          :location :SC-atrium
          :classrank :freshman
          :credits []
          :moves-remaining-in-semester 1000
          :inventory []})

(defn status [player]
        (println (str "You are a " (name (player :classrank)) ". "))
        (let [room (get engineering-campus (player :location))]
                (print (str "You are " (get room :title) ". "))))

(defn to-keywords [commands]
  (mapv keyword (str/split commands #"[.,?! ]+")))

(defn go [dir player]
  (let [location (player :location)
        dest (->> engineering-campus location :dir dir)]
    (if (nil? dest)
      (println "You can't go that way.")
      (assoc-in player [:location] dest))))

 (defn look [player]
   (let [location (player :location)
        classrank (player :classrank)
        room (get engineering-campus (player :location))
        courses-offered (get room :courses)]

        (println "These are the courses offered" (get room :title) ":")
        (println courses-offered))
        player)

 (defn DARS [player]
         (println (player :credits))
         player)

 (defn learn [player]
   (let [location (player :location)
        classrank (player :classrank)
        room (get engineering-campus (player :location))
        courses-offered (get room :courses)
        course (get courses-offered classrank)]

     (if (nil? course)
        (println "There are no classes offered for a" (name classrank) "in this room.")
        (do (println "Congratulations, you've earned credit for" (name course) "!")
            (assoc-in player [:credits] (conj (player :credits) course))))))

(defn respond [player command]
  (match command
          [:north] (go :north player)
          [:n] (go :north player)
          [:south] (go :south player)
          [:s] (go :south player)
          [:east] (go :east player)
          [:e] (go :east player)
          [:west] (go :west player)
          [:w] (go :west player)
          [:down] (go :down player)
          [:d] (go :down player)
          [:up] (go :up player)
          [:u] (go :up player)

          [:learn] (learn player)
          [:DARS] (DARS player)
          [:look] (look player)

         _ (do (println "I don't understand you.")
               player)

         ))

(defn -main [& args]
   (println "\n\n---------- CS @ Illinois: The Text Adventure ----------\n\n")
   (println "Welcome to the University of Illinois at Urbana-Champaign!\nYou are a freshman in one of the most prestigious CS programs in the world.\nOver the next four years, you will navigate forests of up-trees, seas of segfaults, and maelstroms of multiplexors.\nIt will take all your wits to survive.\n\nYour goal is to obtain the necessary prerequisites to advance your class rank. To win, you must graduate in 4 years!")
   (println "\n\n---------- may the finals be ever in your favor ----------\n\n")

  (loop [local-map engineering-campus
         local-player adventurer]
    (let [pl (status local-player)
          _  (println "What do you want to do?")
          command (read-line)]
      (recur local-map (respond local-player (to-keywords command))))))
