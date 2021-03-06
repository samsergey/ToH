<head>
	<link rel="stylesheet" href="../css/textstyles.css">

</style>
  <script type="text/x-mathjax-config">
MathJax.Hub.Config({
  tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
});
</script>
<script type="text/javascript" async
  src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.2/MathJax.js?config=TeX-MML-AM_CHTML">
</script>

</head>
<body>

## Зебра

Говорят, что жизнь похожа на зебру: то белая полоса, то чёрная... А ещё  так бывает, что к одной неприятности добавляется другая, и так непросто в жизни а тут ещё кошка рожать принялась! То густо, то пусто! Одно одному! Но самое печальное, что когда хорошо и в жизни настала светлая полоса, мысли закрадываются нехорошие: ох, не сглазить бы, ох, не придётся ли за счастье расплачиваться...

Знакомое ощущение? Назовём этот закон <dfn>законом зебры:</dfn> 

<div class='law'>
Если всё хорошо и замечательно, то неспроста это, жди неприятностей. Или же: не всё коту масленица. 
</div>

Постараемся выяснить, кроется ли за этим какая-либо закономерность, или нам так только кажется? А если это причуды математики, то можно ли определить характерную длительность или частоту полосок на теле нашей зебры, и от чего она зависит?

В жизни то и дело происходят события. Иногда они вовсе не связаны друг с другом, иногда образуют цепочки причинно-следственных связей. Рассуждения об этих связях, цепочках и предопределённости жизненного пути могут увести нас очень далеко, и мы поговорим о них позже. А пока давайте попробуем, следуя принципу бритвы Оккама, обойтись наименьшим количеством исходных данных для анализа нашего закона. В конце концов, если события образуют связанные логикой и временем цепочки, кластеры, неудивительно, что они будут приводить к чёрным и белым полосам в настроении и в жизни. Так что мы рассмотрим последовательность никак не связанных между собой событий, и посмотрим, что удастся из неё добыть. 

События, которые никак не связаны между собой и происходят во времени случайным образом описываются с помощью хорошо известного <dfn>пуассоновского потока</dfn>. Он соответствует многим случайным явлениям от землетрясений до появления покупателей в магазине. Пуассоновский поток событий характеризуется <dfn>интенсивностью</dfn> или <dfn>плотностью потока</dfn> &mdash; параметром, который определяет ожидаемое число событий в единицу времени. Например, значению параметра $\lambda = 1/7\ дней$ будет соответствовать цепочка случайных событий, в среднем, случающихся раз в неделю. Это не означает, что события будут происходить с *частотой раз в неделю*. Никакой выделенной частоты у последовательности событий может и не быть вовсе. Проще всего представить себе пуассоновский поток с интенсивностью раз в неделю так: в году 52 недели, значит, в год должно произойти около 52 событий (в среднем, за много лет). Если мы выберем 52 случайных равномерно распределённых даты в году, то их можно рассматривать, как моменты возникновения абсолютно независимых пуассоновских событий. 

![Пример построения пуассоновского потока с интенсивностью 1/7 дней. На отрезке в 365 дней случайным образом разбросали никак не связанные между собой 52 события.](figures/happy/PoissonSamples.svg)

При этом о какой-либо периодичности в этих датах речь не идёт, когда пожелают, тогда и случатся. Но и в этом беспорядке статистика может нам показать определённые закономерности. Например, *распределение длительности периодов между событиями* будет вовсе не равномерным.

![Плотность распределения длительностей промежутков между 52 событиями, случайно разбросанными по отрезку в 365 дней.](figures/happy/Exponential.svg)

Распределение длительностей промежутков стремится к *эспоненциальному*, оно показано сплошной линией. У этого распределения максимум (мода) находится в нуле, а среднее значение, равно как раз 7 дням. Более того, стандартное отклонение тоже будет равно 7 дням. Равенство стандартного отклонения и среднего значения &mdash;  характерное свойство экспоненциального распределения. Как видите, эти характеристики вовсе не гарантируют того, что между событиями будет проходить одна неделя, в среднем &mdash; да, но чаще всего &mdash; меньше, к тому же, могут наблюдаться и достаточно долгие промежутки. Наконец, половина всех промежутков будет иметь длительность не превышающую 3 дней &mdash; об этом говорит такая характеристика распределения, как медиана. Теперь видно, что *интенсивность* потока и его *частота* совсем не одно и тоже, это очень важное замечание!

Продолжим моделировать жизнь. Для справедливости, положим, что хорошие и плохие события происходят равновероятно, но яркие, значимые события случаются существенно реже мелких и незначительных. Пусть это будет *нормальная жизнь*, в которой эмоциональная окраска событий подчиняется *нормальному* (Гауссовому) распределению. Вот как может выглядеть год синтетической судьбы, как череды случайных абсолютно независимых жизненных перепитий:

![happyFig1](figures/happy/fig1.svg)

Пока никаких полос не наблюдается, вместо этого есть некий шум. Каждое событие проходит бесследно, ничего не оставляя ни в памяти, ни в настроении. Так не бывает. Давайте наделим нашего модельного героя памятью, для начала, идеальной. Каждое событие пусть навсегда врежется ему в память и в настроение, соответственно, либо улучшая, либо ухудшая его. Вот какую картинку мы можем получить, пронаблюдав за судьбой нашего героя на протяжении десяти лет. 

![happyFig2](figures/happy/fig2.svg)
 
Ну, что же, мы уже видим какие-то полосы, но картинка вышла не шибко радостной. Наш герой после череды смен настроения впал в глубочайшую депрессию. Жаль. Попробуем ещё  несколько судеб?

happyFig3

Все они испытывают череду светлых и тёмных полос, но надолго увязают либо в беспросветной тоске, либо в запредельном счастье. Так бывает, конечно, но это явно ненормально.

Наши модельные судьбы мы описали очень примечательным процессом, он зовется <dfn>случайным блужданием</dfn> и имеет ряд удивительных свойств, среди которых &mdash; <dfn>самоподобие</dfn>, то есть, отсутствие какого-либо характерного временного масштаба. Кроме того, получив в своё распоряжение неограниченное время, случайное блуждание способно увести вас неограниченно далеко, и более того, оно обязательно уведёт вас на любое наперёд заданное расстояние о начального значения! Это значит, что какими бы ни были хорошими ваши дела, если они подчинены случайному блужданию, они обязательно скатятся до нуля и уйдут ниже, это просто вопрос времени! 

Нет, так жить не годится! Похоже, идеальная эмоциональная память это не очень хорошо. Наши герои не забывают ничего и тщательно хранят в памяти всё, даже самые давние события! На их настроении в старости влияет горе от поломанной игрушки в детстве или радость от поцелуя в юности. Причём все последующие поцелуи и игрушки имеют для них такую же важность. Бедолаги! Надо их спасать.
Эмоции со временем стихают, горе притупляется, радость, увы, тоже. Забывание, во многом, подобно остыванию, диффузии или замедлению движения в вязкой жидкости, поэтому разумно моделировать его подобным образом. Перечисленные процессы называются <dfn>процессами релаксации</dfn>. Наделим же и наших бедолаг способностью к релаксации! 

Релаксирующая система возвращается к равновесному состоянию, причём, тем быстрее, чем больше отклонение от равновесия. Это свойство можно промоделировать геометрической прогрессией, или экспоненциальным законом. Придётся ввести в нашу модель новый параметр &mdash; <dfn>скорость забывания</dfn> $\mu$. Его можно выразить через время (в отсчетах нашей модели), за которое уровень эмоции уменьшится достаточно сильно. Например для $\mu = 1/60\ дней$ эмоциональный след от события уменьшится на порядок через два месяца. И вот жизнь стала по-хорошему "полосатой"! Меняя степень забывчивости мы можем получить более или менее эмоционально уравновешенных подопытных. Кажется, мы нашли источник зеброобразности! Это, во-первых, случайные блуждания, склонные к расползанию во все стороны, и, во-вторых, целительная забывчивость, возвращающая настроение в норму.

Давайте изучим свойства полученных нами "синтетических" житейских полос. Построим гистограмму, показывающую распределение их длительностей для длиннющей жизни (или для множества обычных) с параметрами $\lambda = 1/7, \mu = 1/60$. 

happyFig4

Первое, что бросается в глаза &mdash; максимум распределения (мода) находится вблизи нуля, значит, чаще всего периоды счастья и несчастья очень коротки! Это обнадёживает, однако встречаются периоды длительностью более года (их доля составляет %). В среднем же, продолжительность периодов составляет $34$ дня, со средним отклонением в $35$ дней. Это совпадение, как и форма распределения подсказывает, что распределение близко к экспоненциальному. Экспоненциальное раcпределение длительностей полос в жизни означает, что смену настроений можно рассматривать, как пуассоновский поток, то есть как цепочку независимых случайных событий, не имеющих выделенной частоты, но случающихся с некторой известной интенсивностью. Например, в рассмотренном нами примере тёмные и светлые полосы сменяются с интенсивностью около $1/34$, то есть, примерно раз в месяц, но всё равно, больше всего в жизни наблюдается коротких периодов. Половина их не длинее десяти дней, об этом говорит значение медианы для наблюдаемого нами распределения. 

Однако, пуассоновским потоком последовательность смены полос быть не может. Дело в том, что эти смены не вляются независимымы. Из-за того, что вероятность вернуться к нулевому значению достаточно велика при небольшом удалении от нуля, смены полос группируются в достаточно плотные кластеры. По окончании очередной полосы с большой вероятностью последует ещё несколько коротких полос. Последовательность полос подобна последовательности землетрясений в каком-либо сейсмоактивном районе Земли, там тоже наблюдается очень похожая кластеризация. Можно показать, что эти процессы имеют сходные спектральные характеристики, но это уведёт нас далеко от нашей основной темы.

В случае отсутствия "памяти" (для $\mu=0$), распределение перестаёт быть экспоненциально убывающим и становится степенным (пропорциональным степени $-3/2$). 

happyFigRWDist

Статистики говорят, что у таких распределений <dfn>тяжёлый хвост</dfn>, делающий вполне вероятными очень большие отклонения от среднего значения, мы наблюдали их в виде долгих "погружениий" в то иное настроение. У распределения длительности периодов есть одно, на первый взгляд, странное свойство: для него не определены ни среднее значение (математическое ожидание), ни стандартное отклонение, ни медиана. Дело в том, что все эти характеристики вычисляются исходя из площади под кривой плотности распределения, а она -- бесконечна. Посмотрите, что произойдёт при попытке вычислить среднее значение длительности меандров случайного блуждания:

happyFigRWMean

Огромные скачки, происходящие из тяжёлого хвоста то и дело сбивают значение среднего и последовательность усреднений не сходится ни к какому пределу. Мы говорили, что случайное блуждание самоподобно, это значит, что оно не имеет какого-либо собственного масштаба времени. Именно это и отражается в невозможности вычислить среднее значение длительности меандров.

Мы моделировали приспосабливаемость к житейским неурядицам с помощью релаксации, или затухания эмоциональных всплесков. Можно истолковать этот процесс другим образом, как приспосабливаемость человека к жизненным обстоятельствам. При обработке зашумлённых сигналов или последовательностей для сглаживания и выделения полезного сигнала используют <dfn>метод скользящего среднего</dfn>, рассматривая в каждый момент не сам сигнал, а усреднённое значение сигнала в некоторый промежуток времени. Таким образом удаётся избавиться от шума и получить представление о долговременных тенденциях сигнала. Применяя такое усреднение к житейским неурядицам, мы можем моделировать приспосабливаемость человека к обстоятельствам. И во время войн люди влюбляются и находят повод для радости, так же как не безоблачна жизнь богатых бездельников: смещается норма, от которой настроение отклоняется в ту или иную сторону. Рассматривая разницу между последовательность эмоций и сглаженной линией фона, мы получим такую же картину, полос, какую дала нам предыдущая модель, с теми же статистическими характеристиками. Это неудивительно, ведь концептуально они практически не отличаются, описывая систему с релаксацией.

happyFigHappyEMA

Какие выводы можно сделать из нашего небольшого исследования? Череда светлых и тёмных полос в жизни не иллюзия, они есть на самом деле. Но в них нет собенных закономерностей. Чаще всего они коротки, но бывают и затяжными, причём короткие периоды склонные группироваться, образуя пёструю череду частых смен настроения. А вот склонность "застревать" в том или ином настроении зависит от лёгкости характера и способности отпускать прошлое. Более того, если события в жизни будут случаться редко, то жизнь станет серой чередой исчезающих в прошлом воспоминаний. Так что в наших интересах запоминать прожитое. Но в наших же силах сделать так, чтобы жизнь не становилась *случайным блужданием*. Мы можем сделать так, чтобы событий хороших становилось побольше и происходили они почаще, пусть даже они будут и незначительными. Лыжная прогулка, улыбка незнакомой девушке (если вы парень), билет на концерт, чашка горячего шоколада в холодный день, всё это поможет создавать положительный тренд и продлит светлую полосу в жизни. Правда вслед за трендом потянется и среднее значение, так что неизбежные грустные события обязательно сменят настроение. Но не надо винить в этом своё счастье. Это не расплата за него, и не сглаз. А тем завистникам, кто станет вам говорить, что за всё придётся платить, говорите красивую фразу: "Склонность к колебаниям при стохастическом внешнем воздействии -- это свойство релаксирующих систем."

</body>
