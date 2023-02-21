;;; -*- lexical-binding: t; -*-
;;;
;;; Example "open in ema" command for Ex06_Markdown

(defvar ema-ws-address "ws://127.0.0.1:9160")

(defvar ema-ws--conn nil)

(defun ema-ws-connect ()
  (interactive)
  (require 'websocket)
  (unless ema-ws--conn
    (websocket-open
     ema-ws-address
     :on-open (lambda (ws) (message "ema ws: connected") (setq ema-ws--conn ws))
     :on-close (lambda (_) (message "ema ws: disconnected") (setq ema-ws--conn nil)))))

(defun ema-ws-disconnect ()
  (interactive)
  (require 'websocket)
  (when ema-ws--conn (websocket-close ema-ws--conn)))

(defun open-in-ema ()
  (interactive)
  (ema-ws-connect)
  (when ema-ws--conn
    (when-let ((fp (buffer-file-name)))
      (websocket-send-text ema-ws--conn fp))))
