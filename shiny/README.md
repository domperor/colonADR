
# colonADR_shiny

<!-- badges: start -->
<!-- badges: end -->

**For English description, please scroll to the bottom of this page.**

## 説明

このアプリでは，内視鏡の質の指標である

- APC：adenoma per colonoscopy
- ADR：adenoma detection rate
- PPC：polyp per colonoscopy
- PDR：polyp detection rate
- ACN DR：advanced colorectal neoplasm detection rate
- ACN per colonoscopy：advanced colorectal neoplasm per colonoscopy

をJEDスタイルの内視鏡所見データベースから計算することができます。現在日本語（エンコーディング：UTF-8）のみの対応です。

なお，デモ版は<a href="https://domperor.shinyapps.io/apcadrjed_shiny/" target="_blank" rel="noopener noreferrer">Web（shinyapps.io）から，手軽にトライしていただけます</a>。ただし，傍受のリスクを避けるため，機微のある個人情報を扱う場合はローカル版を使用するようにしてください。

![Web（shinyapps.io）から，手軽にトライしていただけます](https://domperor.shinyapps.io/apcadrjed_shiny/)

## ローカル版のインストール方法

1. このアプリはRStudio Desktop上で動作します。<a href="https://posit.co/download/rstudio-desktop/" target="_blank" rel="noopener noreferrer">まずはこちらからRStudio Desktopをダウンロードしてください</a>。

2. <a href="./colonADRapp.R" target="_blank" rel="noopener noreferrer">このフォルダ中のcolonADR.appをダウンロードしてください</a>。ダウンロードボタンの位置：

<img width="750" alt="ダウンロードボタンの位置画像" src="./img/githubraw_dl.png">


一内視鏡中の所見は「|」で区切ってください。


## Description

This app calculate APC (adenoma per colonoscopy), ADR (adenoma detection rate), polyp per colonoscopy (PPC), polyp detection rate (PDR), advanced colorectal neoplasm detection rate (ACN DR), and advanced colorectal neoplasm per colonoscopy (ACN per colonoscopy) from JED-style database. You can try it on [shinyapps.io](https://domperor.shinyapps.io/apcadrjed_shiny/).

