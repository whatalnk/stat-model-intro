---
title: Jupyter インストール
---
## Python
### Windows
* [Miniconda](http://conda.pydata.org/miniconda.html) をインストールする
    * Python 3.5

### Mac
* [Homebrew](http://brew.sh/) をインストールする

```
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
```

* Homebrew から [pyenv](https://github.com/yyuu/pyenv) をインストールする

```
brew update
brew install pyenv
```

* pyenv からMiniconda をインストールする

```
pyenv install miniconda3-latest
```

## [Jupyter](http://jupyter.org/)
```
conda install jupyter
```

## [IRkernel](https://irkernel.github.io/)
* パッケージインストール

R から，

```
install.packages(c('repr', 'IRdisplay', 'evaluate', 'crayon', 'pbdZMQ', 'devtools', 'uuid', 'digest'))
devtools::install_github('IRkernel/IRkernel')
```

* kernel の登録

Windowsの人は普通にR（or RStudio）から，

Mac の人は Terminal から R を起動して，

```
IRkernel::installspec()
```
