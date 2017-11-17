(ns async-tea-party.core
	(:require [clojure.core.async
		:refer [>! <! >!! <!! chan close! go go-loop alts!]]))

