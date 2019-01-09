;;;; sound-viz.lisp 
;;
;; Copyright (c) 2019 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package #:sound-viz)

(defun seconds-since (start)
  (/ (local-time-duration:duration-as
      (local-time-duration:timestamp-difference
       (local-time:now) start)
      :nsec)
     1000000000.0))

(defun live-listen (&key
                      (duration 0)
                      (input-hw "hw:CARD=2,0")
                      (output-hw "default")
                      (sample-rate 44100)
                      (channel-count 2)
                      (buffer-size 256)
                      (pcm-type '(signed-byte 32)))
  (also-alsa:with-alsa-device (ins input-hw buffer-size pcm-type
                                   :direction :input
                                   :sample-rate sample-rate
                                   :channels-count channel-count)
    (also-alsa:with-alsa-device (outs output-hw buffer-size pcm-type
                                      :direction :output
                                      :sample-rate sample-rate
                                      :channels-count channel-count)
      (let ((start-time (local-time:now)))
        (loop while (or (zerop duration) (< (seconds-since start-time) duration)) do
             (also-alsa:alsa-read ins)
             (also-alsa:copy-buffer ins outs)
             (also-alsa:alsa-write outs))))))



(defclass sound-controller ()
  ((state :initform :stopped)
   (input-device :initarg :input-device :documentation "Audio input device, created with also-alsa:alsa-open.")
   (output-device :initarg :output-device :documentation "Audio output device, created with also-alsa:alsa-open.")
   (audio-thread :documentation "Audio processing thread.")
   (buffer-callback :initarg :buffer-callback :documentation "Callback function to process incoming audio data.")
   (xfer-buffer :documentation "Buffer pre-allocated for transferring data to buffer-callback.")))

(defun create-sound-controller (in-device-name out-device-name &key (callback nil)
                                                                 (sample-rate 44100)
                                                                 (channel-count 2)
                                                                 (buffer-size 256)
                                                                 (pcm-type '(signed-byte 32)))
  (let ((rval (make-instance 'sound-controller
                             :input-device (also-alsa:alsa-open in-device-name
                                                                buffer-size
                                                                pcm-type
                                                                :direction :input
                                                                :sample-rate sample-rate
                                                                :channels-count channel-count)
                             :output-device (also-alsa:alsa-open out-device-name
                                                                 buffer-size
                                                                 pcm-type
                                                                 :direction :output
                                                                 :sample-rate sample-rate
                                                                 :channels-count channel-count))))
    (setf (slot-value rval 'xfer-buffer) (also-alsa:alloc-buffer (slot-value rval 'output-device)))
    (setf (slot-value rval 'audio-thread) (bt:make-thread (lambda () (playback-loop rval))))
    (when callback
      (setf (slot-value rval 'buffer-callback) (lambda () (funcall callback rval))))
    rval))

(defun playback-loop (controller)
  (with-slots (state input-device output-device buffer-callback) controller
    (loop while (not (eql state :destroyed)) do
         (cond ((eql state :playing)
                (also-alsa:alsa-read input-device)
                (also-alsa:copy-buffer input-device output-device)
                (also-alsa:alsa-write output-device))
               (t (sleep (/ 256 44100 )))))))

(defun destroy-sound-controller (controller)
  (with-slots (state input-device output-device audio-thread xfer-buffer) controller
      (setf state :destroyed)
      (bt:join-thread audio-thread)
      (also-alsa:free-buffer xfer-buffer)
      (also-alsa:alsa-close input-device)
      (also-alsa:alsa-close output-device)))

(defun stop-playback (controller)
  (with-slots (state) controller
    (setf state :stopped)))

(defun start-playback (controller)
  (with-slots (state) controller
    (setf state :playing)))
