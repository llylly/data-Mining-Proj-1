## 数据挖掘PROJ1 README

*计43 李林翼(2014011361) 朱祺(2014011336)*

### 文件说明

##### 源代码

+ readDoc.R

  读取新闻数据的相关函数源码

+ process.R

  数据处理的相关函数源码

##### 统计图

+ wordCloud.pdf

  单词云图

+ wordLength.pdf

  单词长度分布图

+ categories.pdf

  每类新闻数量分布图

+ timeLine.pdf

  每月新闻数量分布图

+ innerDistance.pdf

  各个类别内新闻之间的平均相似度

##### 数据集

+ data.csv

  从所有新闻集提取出的数据表

+ wordMatrix.csv

  文章的词袋向量矩阵

+ wordFrequency.csv

  各单词总出现次数统计

+ wordLengthSamples.csv

  单词长度分布序列

+ documentCategory.csv

  文章-类别表

+ categoryFrequency.csv

  各类别新闻数量统计表

+ timeSamples.csv

  新闻发布时间序列

+ similarityMatrix.csv

  各新闻余弦相似度矩阵

+ relativityMatrix.csv

  各类别相似度矩阵

+ innerDistance.csv

  各类别内部新闻相似度表

### 运行说明

1. 新闻数据读入与建立数据框对象

   ```R
   source("readDoc.R")
   extractAll("samples_500/", "data.csv")
   #or
   data <- readAll("samples_500/")
   ```

   通过readDoc.R的extractAll函数，可以从新闻素材文件夹中读取各条新闻，建立数据框对象，并保存到data.csv中。

   通过readDoc.R的readAll函数，可以直接获取建立的数据框对象。

   > extractAll函数调用了readAll函数，readAll函数对目录下的所有带有.xml后缀的文件调用readDoc函数，将收集到的数据框对象的单条记录组合在一起，生成data.frame对象。extractAll函数将生成的data.frame对象写入到了csv文件中。
   >
   > readDoc完成了信息的提取和新闻正文的预处理工作。

2. Corpus对象建立

   ```R
   source("process.R")
   corpus <- getCorpus(data[["content"]])
   ```

   该函数通过data数据框的content列，建立文本库corpus对象。

3. BagOfWords向量

   ```R
   source("process.R")
   bag <- getWordMatrix(corpus)
   ```

   该函数接受之前生成的corpus对象，返回每个单词的词袋向量组成的矩阵。这个矩阵规模较大。

4. 绘制单词云图

   ```R
   source("process.R")
   wordFrequency <- drawWordCloud(corpus)
   ```

   该函数接受之前生成的corpus对象，绘制单词云图，并返回一个记录了各个单词出现频数的表。

5. 绘制单词长度分布图

   ```R
   source("process.R")
   wordLength <- drawWordLength(corpus)
   ```

   该函数接受之前生成的corpus对象，绘制单词长度分布图，并返回各个单词长度的序列。

6. 绘制每个类别的新闻数量分布图

   ```R
   source("process.R")
   categoryFrequencyList <- drawCategories(data)
   ```

   该函数接受之前生成的data对象，绘制每个类别的新闻数量分布图，并返回一个记录了各个类别新闻数量的表。

7. 绘制每个月新闻数量分布图

   ```R
   source("process.R")
   timeSamples <- drawTimeLine(data)
   ```

   该函数接受之前生成的data对象，绘制每个月的新闻数量分布图，并返回各个新闻发布时间的序列。

8. 计算新闻之间的相似度矩阵

   ```R
   source("process.R")
   similarityMatrix <- getSimilarityMat(bag)
   ```

   该函数接受之前生成的新闻BagOfWords向量矩阵，计算各个新闻之间的余弦相似度，返回新闻相似度矩阵。

   因为涉及到所有新闻两两之间相似度的计算，所以这个函数运行的时间较长，因此，也可以使用

   ```R
   similarityMatrix <- read.csv("similarityMatrix.csv")
   similarityMatrix <- similarityMatrix[1:dim(similarityMatrix)[1],2:dim(similarityMatrix)[2]]
   ```

   来直接从文件中读取此矩阵。注意：由于写入到文件中时，R自动给矩阵添加了行序号列，所以读出后，需要将第一列（行序号列）删除出去。

9. 计算类别内新闻相似度及类别间新闻相似度

   ```R
   source("process.R")
   relativityMatrix <- getCrossDistance(data, similarityMatrix)
   ```

   该函数接受之前生成的data对象和similarityMatrix矩阵（新闻相似度矩阵），计算各个类别间以及各个类别内新闻的相似度，返回类别相似度矩阵。

   该矩阵的第一列是各个类别的名称，而后是一个M * M的矩阵，它的第 i 行第 j 列表示第 i 个新闻类别和第 j 个新闻类别的相似度，该类别相似度是两个类别内所有新闻两两相似度的平均。特别地，对角元是第 i 个类别内新闻的相似度，它是类别内各个新闻与类别内其他新闻相似度的平均。

10. 查询两个类别的新闻相似度

    ```R
    source("process.R")
    queryDistance(relativityMatrix, "business", "world")
    ```

    该函数接受relativityMatrix矩阵（类别相似度矩阵），和两个类别的名称，返回这两个类别间的相似度。

    该函数通过查询类别相似度矩阵的元素来实现。对于合法的类别名称，会返回它们之间的相似度或类别内新闻相似度（取决于两个类别名是否相同）；对于不合法的类别名称，返回-1.