(ns chat.core
	(:require [chat.clj-sockets
		:refer [create-server listen read-line write-line close-socket]]))


(def server (listen (create-server 9871)))
