Twitter監視スクリプト
=====================

誰にリムられたか知りたかったので書いた。
大分前から公開してたけど需要無いだろうなと思ってたらTwitterで紹介されてて嬉しかったので***リファクタリング***した。(12/9/28)

使い方
------
**ユーザ名を指定するだけ**

	% gosh main.scm "omegatakuma"
	ツイート数: 96108
	  変化: 0
	フォロー数: 863
	  変化: 0
	フォロワー数: 1617
	  変化: -1
	fav数: 156482
	  変化: 0
	--------詳細--------
	@*****にリムられた

追加
----
- リムられた場合、DMで送信。(cronで動かせ

問題点
------
- 自分がr4sしても「リムられた」と出るのは仕様
- ブロックされても「リムられた」と出るのは仕様