;;; ESP32 DOIT DEVKIT V1 ULISP Samples
;;; Blink Example for onboard LED

(defun blink (x)
  (pinmode 2 t)
  (digitalwrite 2 x)
  (delay 2000)
  (blink (not x)))

(blink t)

~ ;; for finishing evaluation

;; ALL WIFI SAMPLES COMES FROM ULISP WEBSITE, I INCLUDED SOME : http://www.ulisp.com/show?2B22

;The first step before running any of these examples is to connect to a local Wi-Fi access point, with a command such as:
 (wifi-connect "Geronimo" "secret99")

;"10.0.1.28"
;; where Geronimo is the network name, or SSID, and secret99 is the password. It returns the IP address as a string.

;; The following examples all make use of the following function println that prints a string followed by the line ending required by most web protocols: return, newline:

(defun println (x s)
  (princ x s)
  (princ #\return s)
  (princ #\newline s))

;; Connecting to a web page
;; This example webpage connects to the uLisp website, reads the RSS version of the uLisp News page (with XML formatting), and lists it to the Serial Monitor:

(defun webpage ()
  (with-client (s "www.ulisp.com" 80)
    (println "GET /rss?1DQ5 HTTP/1.0" s)
    (println "Host: www.ulisp.com" s)
    (println "Connection: close" s)
    (println "" s)
    (loop (unless (zerop (available s)) (return)))
    (loop
     (when (zerop (available s)) (return))
     (princ (read-line s))
     (terpri))))

;;To run it evaluate:

(webpage)

;; Reading and evaluating a function from a web page
;; The following example decode reads the uLisp definition of a function secret from a web page on the uLisp site, and evaluates it to add it to the uLisp image:

(defun decode ()
  (with-client (s "www.ulisp.com" 80)
    (println "GET /list?21Z0 HTTP/1.0" s)
    (println "Host: www.ulisp.com" s)
    (println "Connection: close" s)
    (println "" s)
    (loop (when (= 1 (length (read-line s))) (return)))
    (eval (read s))
    (secret)))

;;To run it evaluate:

(decode)

;; The loop form reads past the webpage header. The eval reads the function definition from the page, and the form (secret) evaluates it, printing a secret message.

;; The following example reads the ADC input and displays its value on a web page served from the ESP8266 or ESP32.

;;First we run the server:

(wifi-server)

;; Now define the following function webpage:

(defun webpage ()
  (loop
   (with-client (s)
     (loop
      (let ((line (read-line s)))
        (when (null line) (return))
        (print line)))
    (println "HTTP/1.1 200 OK" s)
    (println "Content-Type: text/html" s)
    (println "Connection: close" s)
    (println "" s)
    (princ "<!DOCTYPE HTML><html><body><h1>ADC Value: " s)
    (princ (analogread 0) s)
    (println "</h1></body></html>" s)
    (println "" s))
   (delay 5000)))

;; The call to with-client does nothing and returns nil if there's no web browser trying to connect. Otherwise the loop form reads the request from the web browser and displays it. The println statements then submit the web page to be displayed in the browser.

;; Run webpage by typing:

(webpage)

;; It will wait in a loop waiting for a client to connect.

;; Now go to a web browser and enter the address wifi-connect returned earlier. You should see a page showing the value on ADC 0.

;; You can exit from webpage by typing ~.

;; Creating a soft access point
;; As well as connecting to an existing wi-fi network the ESP8266 or ESP32 can create its own wi-fi network, called a soft access point. You can connect to this from a computer or mobile device, and access a web page served from the soft access point.

;; For example, you could use an ESP8266 or ESP32 to create a wireless sensor that you could interrogate from a mobile phone, even if the sensor wasn't in range of a wireless network.

;; To create a soft access point called Bonzo give the command:

(wifi-softap "Bonzo")

;; "192.168.4.1"
;; The command returns the IP address of the soft access point. If you now connect a computer or mobile device to the network Bonzo, and connect a web browser to the IP address 192.168.4.1, you can access a web page served as in the previous example.

;; I2C Port Scanner

;; The following port scanner prints out the address (in decimal) of each of the I2C devices found on the I2C bus:

(defun scn ()
  (dotimes (p 127)
    (with-i2c (str p)
	      (when str (print p)))))

(scn)
