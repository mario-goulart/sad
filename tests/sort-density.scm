(test-sad "by rank"
          "cat density.txt | sad lines :2;\
           cat density.txt | sad lines 2: | sad split '|' | sad buffer -r |\
           sad sort -C numeric -c 0 | sad format '~a|~9@a|~12@a|~11@a|~22@a~%'"
          "head density.txt -n2 ; tail -10 density.txt | sort -n -t '|' -k 1")

(test-sad "by country"
          "cat density.txt | sad lines :2;\
           cat density.txt | sad lines 2: | sad split '|' | sad buffer -r |\
           sad sort -c 1 | sad format '~a|~9@a|~12@a|~11@a|~22@a~%'"
          "head density.txt -n2 ; tail -10 density.txt | sort -t '|' -k 2")

(test-sad "by country reversed"
          "cat density.txt | sad lines :2;\
           cat density.txt | sad lines 2: | sad split '|' | sad buffer -r |\
           sad sort -r -c 1 | sad format '~a|~9@a|~12@a|~11@a|~22@a~%'"
          "head density.txt -n2 ; tail -10 density.txt | sort -r -t '|' -k 2")

(test-sad "by area"
          "cat density.txt | sad lines :2;\
           cat density.txt | sad lines 2: | sad split '|' | sad buffer -r |\
           sad sort -C numeric -c 3 | sad format '~a|~9@a|~12@a|~11@a|~22@a~%'"
          "head density.txt -n2 ; tail -10 density.txt | sort -n -t '|' -k 4")
