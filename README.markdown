# Kaggle-Titanic

This solves the kaggle tutorial, "[Titanic: Machine Learning from Disaster](https://www.kaggle.com/c/titanic)" using Common Lisp.

- *2015/10/19: I decided to use a naive bayes classifier, at first. However, I don't think it is proper way for this task. I'm only interested to know what rate such "naive" way can reach to as baseline.* 

## Usage

At first, you needs to get "train.csv" and "test.csv" from the above kaggle tutorial (also you needs to signup Kaggle). Then, put them to the "resources" directory under this project. 

This project exports two functions, "main" and "cross-validate".

### The "main" function 

1. Learn using all data in "train.csv".
2. Output classifying results to "resources/result.csv" using all data in "test.csv"

```lisp
(kaggle-titanic:main)
```

### The "cross-validate" function

1. Do k-cross validation using data in "train.csv". (k = 5) 
2. Output the result to standard output.

```lisp
(kaggle-titanic:cross-validate)
```

## Installation

This depends on [cl-naive-bayes](https://github.com/eshamster/cl-naive-bayes) that is not registered to quicklisp repository.

So "git clone" both **this** and **cl-naive-bayes** projects under the proper directory (typically it is "~/quicklisp/local-projects" or "~/.roswell/local-projects"), then

```lisp
(ql:quickload :kaggle-titanic)
```

## Author

* eshamster (hamgoostar@gmail.com)

## Copyright

Copyright (c) 2015 eshamster (hamgoostar@gmail.com)

## License

Licensed by the LLGPL License.
