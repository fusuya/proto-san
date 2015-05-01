;;文字列の比較や文字列の分割で使う
(ql:quickload :cl-ppcre)

;;形態素解析する
;;local-projectsにigoフォルダを移動して(ql:register-local-projects)したら使えた
(ql:quickload :igo)
(igo:load-tagger "/home/aria/opt/ipadic/")

;;randomをランダムにするらしい
(setf *random-state* (make-random-state t))

(defun prompt ()
  (format t "proto > "))

;;ランダム辞書配列
(defparameter *random-dict*
  (make-array 100 :fill-pointer 0))

;;random.txtの文字列を*random-dict*へ入れる
(defun read-random-txt ()
  (with-open-file
    (dict "/home/aria/lispgame/munou/random.txt" ;;random.txtのある場所
          :direction :input)
    (do ((str (read-line dict nil)
              (read-line dict nil)))
        ((null str))
        (vector-push str *random-dict*))))

;;*random-dict*からランダムに文字列を返す
(defun random-res ()
  (prompt)
  (let ((res (aref *random-dict* (random (length *random-dict*)))))
    (format t "~a~%" res)))

;;(入力した文字列)ってなに？とだけ返す
(defun response (input)
  (prompt)
  (format t "~aってなに？~%" input))

;;パターン辞書配列
(defparameter *pattern-dict*
  (make-array 100 :fill-pointer 0))
;;pattern.txtの文字列を*pattern-dict*へ入れる
;;"反応する言葉"　+ "/" + "応答する言葉" で作る
(defun read-pattern-txt ()
  (with-open-file
    (dict "/home/aria/lispgame/munou/pattern.txt" ;;pattern.txtのある場所
          :direction :input)
    (do ((str (read-line dict nil)
              (read-line dict nil)))
        ((null str))
        (vector-push str *pattern-dict*))))

;;ユーザーが入力した文字列にマッチするものがあれば、/の右側にある文字列からランダムに返す
(defun pattern-res (input)
  (loop for i from 0 to (- (length *pattern-dict*) 1)
        finally (random-res);;パターンマッチしなければランダム辞書から
        do
        (let* ((split1 (ppcre:split "/" (aref *pattern-dict* i)))
               (scan-str (ppcre:scan (car split1) input))
               (split2 (ppcre:split ":" (cadr split1)))
               (ptn-res (nth (random (length split2)) split2)))
          (cond
            ((numberp scan-str)
             (prompt)
             (format t "~a~%" ptn-res)
             (return))))))
 
;;quitした時に使う
(defun byebye ()
  (prompt)
  (format t "ばいばい"))

;;入力した文字列を*random-dict*へ追加する
(defun study-input (input)
  (vector-push input *random-dict*))

;;入力した文字列をrandom.txtへ追加する
(defun save-input (input)
  (study-input input)
  (with-open-file
    (out "/home/aria/lispgame/munou/random.txt" ;;random.txtのある場所
         :direction :output
         :if-exists :append)
    (format out "~a~%" input)))

;;入力した文字列が*random-dict*内にある文字列とかぶってないかチェックする
(defun check-input (input)
  (loop
     for i from 0 to (- (length *random-dict*) 1)
     finally (save-input input)
     do
       (if (equal input (aref *random-dict* i))
	   (return))))

;;str-concatで作った文字列をpattern.txtに追加する
(defun save-ptn (strptn)
  (vector-push strptn *pattern-dict*)
  (with-open-file
    (out "/home/aria/lispgame/munou/pattern.txt" ;;pattern.txtのある場所
         :direction :output
         :if-exists :append)
    (format out "~a~%" strptn)))

;;取り出した名詞と入力した文字列を/でくっつける
(defun str-concat (noun input)
  (concatenate 'string noun "/" input))

;;取り出した名詞が*pattern-dict*にかぶってるものがないかチェック
;; /の左側と比較する
;;かぶってるのがあったら入力した文字列を/の右側に:で区切って追加するのもいいかも
(defun check-ptn (noun input)
  (loop for i from 0 to (- (length *pattern-dict*) 1)
        finally (save-ptn (str-concat noun input))
        do
        (let* ((split1 (ppcre:split "/" (aref *pattern-dict* i)))
               (scan-str (ppcre:scan (car split1) noun)))
          (if (numberp scan-str)
              (return (check-input input))))));;かぶってるのがあったらcheck-input

;;入力した文字列に名詞があるか調べる
(defun igotest (input)
  (let* ((igoinput (igo:parse input))
         (noun-p "^名詞,(一般|固有名詞|サ変接続|形容動詞語幹)"))
    (loop for i from 0 to (- (length igoinput) 1)
          do
          (if (numberp (ppcre:scan noun-p (second (nth i igoinput))))
              (check-ptn (car (nth i igoinput)) input)))))

;;(igo:parse "これはテストです")
;;(("これ" "名詞,代名詞,一般,*,*,*,これ,コレ,コレ" 0) ("は" "助詞,係助詞,*,*,*,*,は,ハ,ワ" 2) ("テスト" "名詞,サ変接続,*,*,*,*,テスト,テスト,テスト" 3) ("です" "助動詞,*,*,*,特殊・デス,基本形,です,デス,デス" 6))

;;メイン
(defun proto ()
  (read-random-txt)
  (read-pattern-txt)
  (format t "Unmo System prototype : proto~%")
  (loop
    (format t "> ")
    (let ((input (read-line));;ユーザーが入力する
          (ran (random 8)));;応答するパターンをランダムで決める
      (cond
        ((equal input "quit");;quitと入力すると終了
         (return (byebye)))
        (t
          (cond
            ((= ran 0)
             (response input))
            ((or (= ran 1) (= ran 2))
             (random-res))
            (t (pattern-res input)))
          (igotest input))))))

(proto)
