(ns adventure.core
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str])
  (:gen-class))

(def engineering-campus {
        :SC-atrium {
                :desc ""
                :title "in the atrium of the Thomas M. Siebel Center for Computer Science"
                :dir {:north :SC-1404}
                :courses #{}
                :items #{}}
        :DCL-1320 {
                :desc ""
                :title "in DCL-1320"
                :dir {}
                :courses #{:freshman :CS125, :sophomore :CS233, :senior :CS411}
                :items #{}}
        :SC-1105 {
                :desc ""
                :title "in SC-1105"
                :dir {}
                :courses #{:freshman :CS126}
                :items #{}}
        :ECEB-1002 {
                :desc ""
                :title "in ECEB-1002"
                :dir {}
                :courses #{:freshman :CS173, :sophomore :CS225, :junior :CS374}
                :items #{}}
        :SC-0218 {
                :desc ""
                :title "in SC-0218"
                :dir {}
                :courses #{:sophomore :CS241}
                :items #{}}
        :SC-0216 {
                :desc ""
                :title "in SC-0216"
                :dir {}
                :courses #{:sophomore :CS296-25, :senior :CS440}
                :items #{}}
        :SC-1404 {
                :desc ""
                :title "in SC-1404"
                :dir {}
                :courses #{:junior :CS357, :senior :CS421}
                :items #{}}
        :Grainger {
                :desc ""
                :title "in the Grainger Engineering Library"
                :dir {}
                :courses #{:junior :CS242}
                :items #{}}
        :SC-1304 {
                :desc ""
                :title "in SC-1304"
                :dir {}
                :courses #{:senior :CS210}
                :items #{}}
        :SC-4107 {
                :desc "the virtual reality lab"
                :title "in SC-4107"
                :dir {}
                :courses #{:senior :CS498}
                :items #{}}
        :SC-1210 {
                :desc "the academic advising office"
                :title "in SC-1210"
                :dir {}
                :courses #{}
                :items #{}}
        :SC-1318 {
                :desc "the Women in Computer Science (WCS) office"
                :title "in SC-1318"
                :dir {}
                :courses #{}
                :items #{:pancakes}}
        :SC-1104 {
                :desc "the Association for Computing Machinery (ACM) office"
                :title "in SC-1104"
                :dir {}
                :courses #{}
                :items #{:pizza}}
        :SC-1320 {
                :desc "the Latinos in Computer Science (LCS) office"
                :title "in SC-1320"
                :dir {}
                :courses #{}
                :items #{}}
        :SC-2124 {
                :desc "the CocoaNuts meeting room"
                :title "in SC-2124"
                :dir {}
                :courses #{}
                :items #{:friends}}
   })

(def adventurer {
          :location :SC-atrium
          :classrank :freshman
          :credits #{}
          :moves-remaining-in-semester 1000
          :inventory #{}})

(defn status [player]
        (print (str "You are a " (name (player :classrank)) ". "))
        (let [room (get engineering-campus (player :location))]
                (print (str "You are " (get room :title) ". "))))

(defn to-keywords [commands]
  (mapv keyword (str/split commands #"[.,?! ]+")))

(defn go [dir player]
  (let [location (player :location)
        dest (->> engineering-campus location :dir dir)]
    (if (nil? dest)
      (do (println "You can't go that way.")
          player)
      (assoc-in player [:location] dest))
   )
 )

(defn respond [player command]
  (match command
         (:or [:n] [:north]) (go :north player)
         (:or [:s] [:south]) (go :south player)
         (:or [:e] [:east]) (go :east player)
         (:or [:w] [:west]) (go :west player)

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
