(test-sad "by rank"
          "cat density.txt | sad lines :2;\
           cat density.txt | sad lines 2: | sad split '|' | sad buffer -r |\
           sad sort -C numeric -c 0 | sad format '~a|~9@a|~12@a|~11@a|~22@a~%'"
          "cat <<EOF
Rank    Country         Population   Area (km2)   Density (Pop. per km2)
----+----------------+-------------+------------+-----------------------
1   |   Singapore    |     5703600 |        710 |                   8033
2   |   Bangladesh   |   168730000 |     143998 |                   1172
3   |   Lebanon      |     6855713 |      10452 |                    656
4   |   Taiwan       |    23604265 |      36193 |                    652
5   |   South Korea  |    51780579 |      99538 |                    520
6   |   Rwanda       |    12374397 |      26338 |                    470
7   |   Haiti        |    11577779 |      27065 |                    428
8   |   Netherlands  |    17470000 |      41526 |                    421
9   |   Israel       |     9200000 |      22072 |                    417
10  |   India        |  1363110000 |    3287240 |                    415
EOF")

(test-sad "by country"
          "cat density.txt | sad lines :2;\
           cat density.txt | sad lines 2: | sad split '|' | sad buffer -r |\
           sad sort -c 1 | sad format '~a|~9@a|~12@a|~11@a|~22@a~%'"
          "cat <<EOF
Rank    Country         Population   Area (km2)   Density (Pop. per km2)
----+----------------+-------------+------------+-----------------------
2   |   Bangladesh   |   168730000 |     143998 |                   1172
7   |   Haiti        |    11577779 |      27065 |                    428
10  |   India        |  1363110000 |    3287240 |                    415
9   |   Israel       |     9200000 |      22072 |                    417
3   |   Lebanon      |     6855713 |      10452 |                    656
8   |   Netherlands  |    17470000 |      41526 |                    421
6   |   Rwanda       |    12374397 |      26338 |                    470
1   |   Singapore    |     5703600 |        710 |                   8033
5   |   South Korea  |    51780579 |      99538 |                    520
4   |   Taiwan       |    23604265 |      36193 |                    652
EOF")

(test-sad "by country reversed"
          "cat density.txt | sad lines :2;\
           cat density.txt | sad lines 2: | sad split '|' | sad buffer -r |\
           sad sort -r -c 1 | sad format '~a|~9@a|~12@a|~11@a|~22@a~%'"
          "cat <<EOF
Rank    Country         Population   Area (km2)   Density (Pop. per km2)
----+----------------+-------------+------------+-----------------------
4   |   Taiwan       |    23604265 |      36193 |                    652
5   |   South Korea  |    51780579 |      99538 |                    520
1   |   Singapore    |     5703600 |        710 |                   8033
6   |   Rwanda       |    12374397 |      26338 |                    470
8   |   Netherlands  |    17470000 |      41526 |                    421
3   |   Lebanon      |     6855713 |      10452 |                    656
9   |   Israel       |     9200000 |      22072 |                    417
10  |   India        |  1363110000 |    3287240 |                    415
7   |   Haiti        |    11577779 |      27065 |                    428
2   |   Bangladesh   |   168730000 |     143998 |                   1172
EOF")

(test-sad "by area"
          "cat density.txt | sad lines :2;\
           cat density.txt | sad lines 2: | sad split '|' | sad buffer -r |\
           sad sort -C numeric -c 3 | sad format '~a|~9@a|~12@a|~11@a|~22@a~%'"
          "cat <<EOF
Rank    Country         Population   Area (km2)   Density (Pop. per km2)
----+----------------+-------------+------------+-----------------------
1   |   Singapore    |     5703600 |        710 |                   8033
3   |   Lebanon      |     6855713 |      10452 |                    656
9   |   Israel       |     9200000 |      22072 |                    417
6   |   Rwanda       |    12374397 |      26338 |                    470
7   |   Haiti        |    11577779 |      27065 |                    428
4   |   Taiwan       |    23604265 |      36193 |                    652
8   |   Netherlands  |    17470000 |      41526 |                    421
5   |   South Korea  |    51780579 |      99538 |                    520
2   |   Bangladesh   |   168730000 |     143998 |                   1172
10  |   India        |  1363110000 |    3287240 |                    415
EOF")
