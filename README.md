# Project 1

- readDoc

  - head
    - **title**
    - docdata
      - **docid**
      - identified-content
        - **locations**
    - publication_**day_of_month**
    - publication_**month**
    - publication_**year**
    - online_sections(**categories**)
    - date.publication(**correction_data**)

- getCorpus:

  - input：vector of content
  - output：corpus，which preprocesses using tm

- getBagofWords

  - input：corpus

  - output：list，bagofwords of docs，using DocumentTermMatrix()

    - example：

    - [[1]]
      [[1]]$fd
      [1] 1

      [[1]]$f
      [1] 2


      [[2]]
      [[2]]$fd
      [1] 1

      [[2]]$f
      [1] 2