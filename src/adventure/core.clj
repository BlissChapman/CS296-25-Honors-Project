(ns adventure.core
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str])
  (:gen-class))

(def engineering-campus {
        :SC-atrium {
                :desc ""
                :title "in the atrium"
                :dir {:northeast :SC-1404}
                :contents #{}}
        :DCL-1320 {
                :desc ""
                :title ""
                :dir {}
                :contents #{:freshman :CS125, :sophomore :CS233, :senior :CS411}}
        :SC-1105 {
                :desc ""
                :title ""
                :dir {}
                :contents #{:freshman :CS126}}
        :ECEB-1002 {
                :desc ""
                :title ""
                :dir {}
                :contents #{:freshman :CS173, :sophomore :CS225, :junior :CS374}}
        :SC-0218 {
                :desc ""
                :title ""
                :dir {}
                :contents #{:sophomore :CS241}}
        :SC-0216 {
                :desc ""
                :title ""
                :dir {}
                :contents #{:sophomore :CS296-25, :senior :CS440}}
        :SC-1404 {
                :desc ""
                :title ""
                :dir {}
                :contents #{:junior :CS357, :senior :CS421}}
        :Grainger {
                :desc ""
                :title ""
                :dir {}
                :contents #{:junior :CS242}}
        :SC-1304 {
                :desc ""
                :title ""
                :dir {}
                :contents #{:senior :CS210}}
        :SC-4107 {
                :desc ""
                :title ""
                :dir {}
                :contents #{:senior :CS498}}
        :SC-1210 {
                :desc ""
                :title "in the academic office"
                :dir {}
                :contents #{}}
        :SC-1318 {
                :desc ""
                :title "in the Women in Computer Science (WCS) office"
                :dir {}
                :contents #{}}
        :SC-1104 {
                :desc ""
                :title "in the Association for Computing Machinery (ACM) office"
                :dir {}
                :contents #{}}
        :SC-1320 {
                :desc ""
                :title "in the Latinos in Computer Science (LCS) office"
                :dir {}
                :contents #{}}
        :SC-2124 {
                :desc ""
                :title "in the CocoaNuts meeting room"
                :dir {}
                :contents #{}}
   })

(def adventurer
  {:location :SC-atrium
   :inventory #{}
   :tick 0
   :seen #{}})

(defn status [player]
  (let [location (player :location)]
    (print (str "You are " (->  location :title) ". "))
    (when-not ((player :seen) location)
      (print (-> engineering-campus location :desc)))
    (update-in player [:seen] #(conj % location))))

(defn to-keywords [commands]
  (mapv keyword (str/split commands #"[.,?! ]+")))

(defn go [dir player]
  (let [location (player :location)
        dest (->> engineering-campus location :dir dir)]
    (if (nil? dest)
      (do (println "You can't go that way.")
          player)
      (assoc-in player [:location] dest))))

(defn tock [player]
  (update-in player [:tick] inc))

(defn respond [player command]
  (match command
         [:look] (update-in player [:seen] #(disj % (-> player :location)))
         (:or [:n] [:north] ) (go :north player)
         [:south] (go :south player)

         _ (do (println "I don't understand you.")
               player)

         ))

(defn -main [& args]
   (println "\n\n---------- CS @ Illinois: The Text Adventure ----------\n\n")
   (println "Welcome to the University of Illinois at Urbana-Champaign! You are a freshman in one of the most prestigious CS programs in the world. Over the next four years, you will navigate forests of up-trees, seas of segfaults, and maelstroms of multiplexors. It will take all your wits to survive.\n\nYour goal is to obtain the necessary prerequisites to advance your class rank. To win, you must graduate in 4 years!")
   (println "\n\n---------- may the finals be ever in your favor ----------\n\n")

  (loop [local-map engineering-campus
         local-player adventurer]
    (let [pl (status local-player)
          _  (println "What do you want to do?")
          command (read-line)]
      (recur local-map (respond pl (to-keywords command))))))
