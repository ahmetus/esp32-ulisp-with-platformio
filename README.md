# esp32-ulisp-with-platformio
Ulisp ESP32 Setup Code for PlatformIO

Video demonstration : https://youtu.be/i8rEc95dyhs

Emacs setup Code :

<code>
(defvar port "/dev/ttyUSB0" "esp32doit-devkit-v1")
(defvar bauds 9600 "Bps")

(defun sb-open()
  (let ((serial-buffer (serial-term port bauds)))
  (with-current-buffer
      (rename-buffer "*inferior-lisp*")
    (term-line-mode)
    (setq inferior-lisp-buffer serial-buffer))))

(sb-open)
<code>

