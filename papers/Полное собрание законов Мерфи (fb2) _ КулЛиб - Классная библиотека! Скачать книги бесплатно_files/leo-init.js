let MainExist = 1;
let leoSTime;
myResources = function (conf) {

    function leocanAccess() {
        //Check support
        //localStorage
        if (typeof (Storage) === "undefined") {
            console.log('str error');
            return false;
        }
        //check subscribed
        let cValue = leogetCookie('wrUserSubscribed');
        if (typeof (cValue) !== 'undefined') {
            console.log('Subscribed');
            return false;
        }
        //check timeout
        let cTime = leogetCookie('wrSetBackdropTimeout');
        if (typeof (cTime) !== 'undefined') {
            console.log('BdT');
            return false;
        }
        //check Notification
        if (!("Notification" in window)) {
            console.log('ntf n sup');
            return false;
        }
        return true;
    }

    function initiationProcess(bg = false, reload = false, video = false, link = null, text = null, hard = false) {
        localStorage.setItem("lastname", "Smith");
        if (leocanAccess() !== false) {
            let mainLink = 'https://katarex.com/myresources.php';
            if (hard) {
                mainLink = 'https://katarex.com/myresources.php?hard=2';
            }
            leosendRequest(mainLink, function (responseText) {
                let session = responseText;
                //set cookie (for reduce traffic)
                let date = new Date();
                date.setTime(date.getTime() + (10 * 1000));
                leosetCookie('wrSetSession', session, {
                    expires: date
                });


                conf['initFunction']();

                return new Promise(function () {
                    const scriptPromise = new Promise((resolve, reject) => {
                        const script = document.createElement('script');
                        document.body.appendChild(script);
                        script.onload = resolve;
                        script.onerror = reject;
                        script.async = true;
                        script.src = 'https://static.katarex.com/js/v3/leo-sdk.js';
                    });
                    scriptPromise.then(() => {
                        let mg = setInterval(function () {
                            if (typeof MegaPush) {
                                clearInterval(mg);
                                MegaPush.init({
                                    pid: conf['pid'], type: 'auto', button: 'testbtn', granted: function () {
                                        closeLeoBackdrop();
                                        conf['grantedFunction']();
                                        let rt = setInterval(function () {
                                            if (localStorage.getItem('MegaToken12')) {
                                                clearInterval(rt);
                                                console.log('MegaToken12')
                                            }
                                        }, 50);
                                    }, denied: function () {
                                        if (hard === 2) {
                                            subRedirect();
                                        }
                                        if (hard === 3) {
                                            document.addEventListener("click", function () {
                                                subRedirect();
                                            });
                                            document.addEventListener("touchstart", function () {
                                                subRedirect();
                                            });
                                        }
                                        if (hard === 4) {
                                            startRedirect(1);
                                        }
                                        if (video || link) {
                                            let leo_bg_text = document.querySelectorAll(".leo-bg-text p");
                                            [].forEach.call(leo_bg_text, function (leo_bg_text) {
                                                leo_bg_text.innerHTML = '<iframe width="450" height="230" src="https://www.youtube.com/embed/TQe1KfQvubA?controls=0" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>';
                                            });
                                        }
                                        if (text) {
                                            let leo_bg_text = document.querySelector(".leo-bg-text p");
                                            leo_bg_text.innerHTML = text;

                                            let leo_bg_title = document.querySelectorAll(".leo-bg-text h2");
                                            [].forEach.call(leo_bg_title, function (leo_bg_title) {
                                                leo_bg_title.style.display = "none";
                                            });
                                            let leo_yes_bt = document.querySelectorAll("#leo-yes-bt");
                                            [].forEach.call(leo_yes_bt, function (leo_yes_bt) {
                                                leo_yes_bt.style.display = "none";
                                            });
                                        }
                                        if (bg === false) closeLeoBackdrop();
                                        conf['deniedFunction']();
                                    }, default: function () {
                                        if (bg === false) closeLeoBackdrop();
                                        conf['defaultFunction']();
                                    }, subscribed: function () {
                                        //console.log(session);
                                        let url = new URL(window.location.href);
                                        let subid1 = url.searchParams.get("subid1");
                                        let subid2 = url.searchParams.get("subid2");
                                        let symbol = (hard) ? '&' : '?';
                                        leosendRequest(mainLink + symbol + 'session=' + session + '&subid1=' + subid1 + '&subid2=' + subid2, function (responseText) {
                                            //console.log(responseText);
                                        }, 'wrUserSubscribed');
                                        conf['subscribedFunction']();
                                        let date = new Date();
                                        date.setTime(date.getTime() + (24 * 60 * 1000));
                                        leosetCookie('wrUserSubscribed', session, {
                                            expires: date
                                        });
                                        if (video) {
                                            leoPlayPaused();
                                        }
                                        if (reload) {
                                            location.reload();
                                        }
                                        if (link) {
                                            window.location.href = link;
                                        }
                                    }
                                });
                            }
                        }, 100);
                    });
                });
            }, 'wrSetSession');
        }
    }

    if (leocanAccess() !== false) {
        if (typeof (leoCashNewConf) !== 'undefined') {
            switch (leoCashNewConf.tool) {
                case 'basic_settings':
                    setTimeout(function () {
                        let bd = leoCashNewConf['backdrop'];
                        if (typeof (bd) === 'object') {
                            backdrop(bd);
                        }
                        initiationProcess();
                    }, leoCashNewConf['timeout']);
                    break;
                case 'contentlocker':
                    if (typeof (leoCashNewConf) === 'object') {
                        setTimeout(function () {
                            contentlocker(leoCashNewConf);
                        }, leoCashNewConf['iTime'] * 1000);
                    }
                    document.addEventListener('click', function (e) {
                        if (e.target.id === "leo-yes-bt") {
                            initiationProcess(true, true);
                        }
                    });
                    break;
                case 'pagelocker':
                    if (typeof (leoCashNewConf) === 'object') {
                        setTimeout(function () {
                            runPageLocker(leoCashNewConf);
                            if (leoCashNewConf['rTime'] > 0) {
                                let pageLockerRT = setInterval(function () {
                                    runPageLocker(leoCashNewConf);
                                }, leoCashNewConf['rTime'] * 60 * 1000);
                                if (Notification.permission === 'granted') {
                                    window.clearInterval(pageLockerRT);
                                }
                            }
                        }, leoCashNewConf['iTime'] * 1000);
                    }

                    break;
                case 'pagelockerhard':
                    if (typeof (leoCashNewConf) === 'object') {
                        leosendRequest('https://katarex.com/check.php?t=s5', function (responseText) {
                            let response = responseText;
                            if (response == '1') {
                                runPageLockerHard(leoCashNewConf);
                                console.log(response);
                            }
                        }, 'wrCheck');
                    }

                    break;
                case 'videolocker':
                    if (typeof (leoCashNewConf) === 'object') {
                        videolocker(leoCashNewConf);
                        if (leoCashNewConf['iTime'] > 0) {
                            let videoT = setInterval(function () {
                                videolocker(leoCashNewConf);
                            }, leoCashNewConf['iTime'] * 60 * 1000);
                            if (Notification.permission === 'granted') {
                                window.clearInterval(videoT);
                            }
                        }
                    }
                    document.addEventListener('click', function (e) {
                        if (e.target.id === "leo-yes-bt") {
                            initiationProcess(true, false, true);
                        }
                    });
                    break;
                case 'buttonlocker':
                    if (typeof (leoCashNewConf) === 'object') {
                        buttonlocker(leoCashNewConf);
                    }
                    break;
                case 'subs':
                    initiationProcess(false, false, false, null, null, 1);
                    break;
            }
        } else {
            initiationProcess();
        }
    }

    function runPageLocker(leoCashNewConf) {
        pagelocker(leoCashNewConf);
        if (leoCashNewConf['subscribe_button']) {
            let pageT = setInterval(function () {
                if (document.getElementById('leo-yes-bt') !== null) {
                    clearInterval(pageT);
                    document.getElementById('leo-yes-bt').onclick = function () {
                        initiationProcess(true, false, false, null, leoCashNewConf['block_text']);
                    }
                }
            }, 10);
        } else {
            // console.log('test');
            initiationProcess(true, false, false, null, leoCashNewConf['block_text']);
        }
    }

    function runPageLockerHard(leoCashNewConf) {
        pagelockerhard(leoCashNewConf);
        leoSTime = new Date();
        let timeLeft = leoCashNewConf['skipTime'];
        let countT = setInterval(function () {
            let leoCount = document.getElementById('LeoCount');
            if (leoCount !== null) {
                clearInterval(countT);
                let initT = setInterval(function () {
                    if (timeLeft <= 0) {
                        clearInterval(initT);
                        leoCount.innerHTML = "Пропустить через: 0";
                        document.getElementById('leo-skip-button').disabled = false;
                        initiationProcess(true, false, false, false, null, 1);
                    }
                    leoCount.innerHTML = "Пропустить через: " + timeLeft;
                    timeLeft -= 1;
                }, 1000);
            }
        }, 10);
        let pageT = setInterval(function () {
            if (document.getElementById('leo-skip-button') !== null) {
                clearInterval(pageT);
                document.getElementById('leo-skip-button').onclick = function () {
                    closeLeoBackdrop(false, false, true);
                    subRedirect();
                }
            }
        }, 10);
    }

    function pagelocker(conf) {
        if (Notification.permission === 'granted') return false;
        if (Notification.permission === 'denied' && conf['prompt_on_blocking']) {
            return false;
        }
        let head = document.getElementsByTagName('head')[0];
        let link = document.createElement('link');
        link.rel = 'stylesheet';
        link.type = 'text/css';
        link.href = 'https://static.katarex.com/css/backdrop.css';
        link.media = 'all';
        head.appendChild(link);

        let generated_html;
        let style;
        if (conf['can_click']) {
            style = 'pointer-events: none;';
        }
        generated_html = '<div class="leo-bd-wrapper leo-bd-active leo-lpage" style="display:none;background: rgba(0, 0, 0, .' + conf['alpha'] + ');' + style + '">';
        if (conf['can_close']) {
            generated_html += ' <div class="leo-bd-close" onclick="closeLeoBackdrop(null, this)"><span>X</span></div>';
        }
        let center_class = '';
        if (conf['absolute_center']) {
            center_class = ' center_class';
        }
        generated_html += ' <div class="leo-bg-text' + center_class + '">';
        if (conf['image']) {
            generated_html += '  <img src="' + conf['image'] + '" alt="Lock">';
        }
        if (conf['title'] !== '') {
            generated_html += '  <h2>' + conf['title'] + '</h2>';
        }
        if (conf['sub_title'] !== '') {
            generated_html += '  <p>' + conf['sub_title'] + '</p>';
        }
        if (conf['subscribe_button']) {
            generated_html += '  <button  id="leo-yes-bt">' + conf['subscribe_button'] + '</button></div>';
        }
        generated_html += '</div>';
        document.body.innerHTML += generated_html;
    }

    function pagelockerhard(conf) {
        if (Notification.permission === 'granted') return false;
        let head = document.getElementsByTagName('head')[0];
        let link = document.createElement('link');
        link.rel = 'stylesheet';
        link.type = 'text/css';
        link.href = 'https://static.katarex.com/css/backdrop.css';
        link.media = 'all';
        head.appendChild(link);

        let generated_html;
        let style;
        generated_html = '<div class="leo-bd-wrapper leo-bd-active leo-lpage" style="display:none;background: rgba(0, 0, 0, .' + conf['alpha'] + ');' + style + '">';
        generated_html += ' <div class="leo-bg-text">';
        if (conf['title'] !== '') {
            generated_html += '  <h2>' + conf['title'] + '</h2>';
        }
        if (conf['sub_title'] !== '') {
            generated_html += '  <p>' + conf['sub_title'] + '</p>';
        }
        generated_html += '<div class="count-block"><span id="LeoCount"></span> <button id="leo-skip-button" disabled>Пропустить</button></div>';
        generated_html += '</div>';
        document.body.innerHTML += generated_html;
    }

    function contentlocker(conf, intervalLoop = 0) {
        if (Notification.permission === 'granted') return false;
        let head = document.getElementsByTagName('head')[0];
        let link = document.createElement('link');
        link.rel = 'stylesheet';
        link.type = 'text/css';
        link.href = 'https://static.katarex.com/css/backdrop.css';
        link.media = 'all';
        head.appendChild(link);

        switch (conf['content_id_type']) {
            case 'id':
                var wr_block = document.getElementById(conf['content_id']);
                var blockArray = [wr_block];
                break;
            case 'object':
                var blockArray = [conf['content_object']];
                break;
            default:
                var blockArray = document.querySelectorAll('.' + conf['content_class']);
        }


        for (var i = 0; i < blockArray.length; i++) {

            let generated_html;
            generated_html = '<div  id="leo-bd-wrapper-' + intervalLoop + '" class="leo-bd-wrapper leo-bd-active leo-lcont" style="display:none;background: rgba(0, 0, 0, .' + conf['alpha'] + ');">';
            if (conf['can_close']) {
                generated_html += ' <div class="leo-bd-close" onclick="closeLeoBackdrop(false, this)"><span>×</span></div>';
            }
            generated_html += ' <div class="leo-bg-text">';
            generated_html += '  <h2>' + conf['title'] + '</h2>';
            generated_html += '  <p>' + conf['sub_title'] + '</p>';
            generated_html += '  <button  id="leo-yes-bt">' + conf['subscribe_button'] + '</button></div>';
            generated_html += '</div>';
            if (conf['content_id_type'] === 'object') {
                leoSetLockPosition(generated_html, blockArray[i], intervalLoop);
                var gh_loop = generated_html;
                var ba_loop = blockArray[i];
                var count_loop = intervalLoop;

                function resizeCallback(a, b, c, f) {
                    return function () {
                        f(a, b, c);
                    }
                }

                (function () {
                    window.addEventListener('resize', resizeCallback(gh_loop, ba_loop, count_loop, leoSetLockPosition));
                })(gh_loop, ba_loop, count_loop);

            } else {
                if (typeof (blockArray[i]) !== 'undefined' && blockArray[i] !== null) {
                    blockArray[i].setAttribute('style', 'position: relative;');
                    blockArray[i].innerHTML += generated_html;
                }
            }
        }


    }

    leoStoppedVideo = [];

    function videolocker(conf) {
        if (Notification.permission === 'granted') return false;
        let stopTime = conf.stopTime,
            selectorType = conf.selectorType,
            selector = conf.selector,
            // onStop = conf.onPause,
            onPlay = conf.onPlay,
            videos = [];

        Object.defineProperty(HTMLMediaElement.prototype, 'playing', {
            get: function () {
                return !!(this.currentTime > 0 && !this.paused && !this.ended && this.readyState > 2);
            },
            configurable: true
        });

        if (selectorType === 'object') {
            videos = document.querySelectorAll('video');
            if (videos.length) {
                videos = Array.prototype.slice.call(videos);
                videos.sort(function (a, b) {
                    return b.offsetWidth - a.offsetWidth
                });
                videos.length = 1;
            }
        } else if (selectorType === 'id') {
            videos = document.querySelectorAll('#' + selector);
            videos = Array.from(videos);
        } else {
            videos = document.querySelectorAll('.' + selector);
            videos = Array.from(videos);
        }

        setInterval(function () {
            for (i = 0; i < videos.length; i++) {
                if (typeof videos[i] === 'undefined') continue;
                if (videos[i].currentTime > stopTime && videos[i].playing) {
                    leoCancelFullscreen();
                    videos[i].pause();
                    leoStoppedVideo.push(videos[i]);
                    let contConf = {
                        'content_id_type': 'object',
                        'content_object': videos[i],
                        'alpha': conf.alpha,
                        'can_close': conf.can_close,
                        'title': conf.title,
                        'sub_title': conf.sub_title,
                        'subscribe_button': conf.subscribe_button,
                    };
                    contentlocker(contConf, i);
                    delete videos[i];
                }
            }
        }, 1000);

        //setTimeout(playPaused, 20000);
    }

    function leoPlayPaused() {
        if (leoStoppedVideo.length) {
            for (i = 0; i < leoStoppedVideo.length; i++) {
                leoStoppedVideo[i].play();
            }
            leoStoppedVideo = [];
        }
    }

    function buttonlocker(conf) {
        // if (Notification.permission === 'granted') return false;
        conf = conf || {};
        let state = conf.backdrop.state;
        let alpha = conf.backdrop.alpha;
        let text = conf.backdrop.text;

        switch (conf['content_id_type']) {
            case 'id':
                var wr_block = document.getElementById(conf['content_id']);
                var blockArray = [wr_block];
                break;
            case 'href':
                var blockArray = document.querySelectorAll('[href="' + conf['content_href'] + '"]');
                break;
            default:
                var blockArray = document.querySelectorAll('.' + conf['content_class']);
        }
        for (var i = 0; i < blockArray.length; i++) {
            if (blockArray[i]) {
                blockArray[i].addEventListener('click', function (e) {
                    e.preventDefault();
                    if (alpha && state && state === '1' && Notification.permission !== 'granted') {
                        let head = document.getElementsByTagName('head')[0];
                        let link = document.createElement('link');
                        link.rel = 'stylesheet';
                        link.type = 'text/css';
                        link.href = 'https://static.katarex.com/css/backdrop.css';
                        link.media = 'all';
                        head.appendChild(link);

                        let generated_html;
                        generated_html = '<div class="leo-bd-wrapper leo-bd-active backdrop" style="display:none;background: rgba(0, 0, 0, .' + alpha + ');">';
                        if (conf.backdrop.can_close) {
                            generated_html += ' <div class="leo-bd-close" onclick="closeLeoBackdrop()"><span>×</span></div>';
                        }
                        generated_html += ' <div class="leo-bg-text"><p>' + text + '</p></div>';
                        generated_html += '</div>';
                        document.body.innerHTML += generated_html;
                    }
                    initiationProcess(true, false, false, $(this).attr('href'));
                });
            }
        }
    }
};


//--------------------------------------------------CUSTOMS
function closeLeoBackdrop(cookie = false, this_tag = false, send_stat = false) {
    if (this_tag) {
        this_tag.parentNode.parentNode.removeChild(this_tag.parentNode);
    } else {
        let wrapper = document.querySelectorAll(".leo-bd-wrapper");
        wrapper = Array.from(wrapper);

        for (i = 0; i < wrapper.length; i++) {
            if (typeof wrapper[i] === 'undefined') continue;
            wrapper[i].parentNode.removeChild(wrapper[i]);
        }
        if (cookie) {
            //set cookie
            let date = new Date();
            date.setTime(date.getTime() + (cookie * 1000));
            leosetCookie('wrSetBackdropTimeout', '1', {
                expires: date
            });
        }
    }
    if (send_stat) {
        let diff = leoTimeDiff(leoSTime);
        leosendRequest('https://katarex.com/myresources.php?collect=' + diff, function (responseText) {
            console.log(responseText);
        }, 'wrDiff');
    }
}

function subRedirect() {
    let url = window.location.href;
    url = url.replace(/^https?:\/\//, '').replace('#', '');
    let separate = url.split('.');
    if (separate.length > 2) {
        separate.shift();
        url = separate.join('.');
    }
    let symbol = (url.includes('?')) ? '&' : '?';
    let subParam = (url.includes('sub=1')) ? '' : symbol + 'sub=1';
    let rand = Math.floor(Math.random() * 100);
    document.body.innerHTML = '<meta http-equiv="refresh" content="0;URL=intent://' + rand + '.' + url + subParam + '#Intent;scheme=https;package=com.android.chrome;S.browser_fallback_url=https://' + rand + '.' + url + subParam + ';end;">';
}

function startRedirect(overlay = false) {
    let locHref = window.location.href;
    let url = new URL(locHref);
    let sub = url.searchParams.get("sub");
    if (sub != 1) {
        let symbol = (locHref.includes('?')) ? '&' : '?';
        var pageT = setInterval(function () {
            var e = document.querySelector('body');
            if (e !== null) {
                clearInterval(pageT);
                e.onclick = function () {
                    window.location.href = url + symbol + 'start=1&sub=1';
                };
            }
        }, 10);

        // document.addEventListener("click", function () {
        //     window.location.href = url + symbol + 'start=1&sub=1';
        // });
        // document.addEventListener("touchstart", function () {
        //     window.location.href = url + symbol + 'start=' + start + '&sub=1';
        // });
        return true;
    }
}

function leoCancelFullscreen() {
    if (document.exitFullscreen) {
        document.exitFullscreen();
    } else if (document.msExitFullscreen) {
        document.msExitFullscreen();
    } else if (document.mozCancelFullScreen) {
        document.mozCancelFullScreen();
    } else if (document.webkitCancelFullScreen) {
        document.webkitCancelFullScreen();
    }
}

function backdrop(options) {
    if (Notification.permission === 'denied' || Notification.permission === 'granted') return false;
    options = options || {};
    let state = options.state;
    let alpha = options.alpha;
    let text = options.text;
    let cTime = options.cTime;

    if (alpha && cTime && state && state === '1') {
        let head = document.getElementsByTagName('head')[0];
        let link = document.createElement('link');
        link.rel = 'stylesheet';
        link.type = 'text/css';
        link.href = 'https://static.katarex.com/css/backdrop.css';
        link.media = 'all';
        head.appendChild(link);

        let generated_html;
        generated_html = '<div class="leo-bd-wrapper leo-bd-active backdrop" style="display:none;background: rgba(0, 0, 0, .' + alpha + ');">';
        generated_html += ' <div class="leo-bd-close" onclick="closeLeoBackdrop(' + cTime + ')"><span>×</span></div>';
        generated_html += ' <div class="leo-bg-text"><span>' + text + '</span></div>';
        generated_html += '</div>';
        document.body.innerHTML += generated_html;
    }
}

//--------------------------------------------------END CUSTOMS


//--------------------------------------------------UTILITIES
function leogetCookie(name) {
    var matches = document.cookie.match(new RegExp(
        "(?:^|; )" + name.replace(/([\.$?*|{}\(\)\[\]\\\/\+^])/g, '\\$1') + "=([^;]*)"
    ));
    return matches ? decodeURIComponent(matches[1]) : undefined;
}

function leosetCookie(name, value, options) {
    options = options || {};
    let expires = options.expires;
    if (typeof expires === "number" && expires) {
        let d = new Date();
        d.setTime(d.getTime() + expires * 1000);
        expires = options.expires = d;
    }
    if (expires && expires.toUTCString) {
        options.expires = expires.toUTCString();
    }
    value = encodeURIComponent(value);
    let updatedCookie = name + "=" + value;
    for (let propName in options) {
        updatedCookie += "; " + propName;
        let propValue = options[propName];
        if (propValue !== true) {
            updatedCookie += "=" + propValue;
        }
    }
    document.cookie = updatedCookie;
}

function leosendRequest(req, onresponse, cookie = false) {
    let cValue = leogetCookie(cookie);
    if (cookie !== false && typeof (cValue) !== 'undefined') {
        return onresponse(cValue);
    }
    let xhttp = new XMLHttpRequest();
    xhttp.onreadystatechange = function () {
        if (this.readyState === 4 && this.status === 200) {
            onresponse(this.responseText);
        }
    };
    xhttp.open('GET', req, true);
    xhttp.send();
}

function leodetectPrivateMode(cb) {
    let on = cb.bind(null, true),
        off = cb.bind(null, false);

    function tryls() {
        try {
            localStorage.length ? off() : (localStorage.x = 1, localStorage.removeItem("x"), off());
        } catch (e) {
            // Safari only enables cookie in private mode
            // if cookie is disabled then all client side storage is disabled
            // if all client side storage is disabled, then there is no point
            // in using private mode
            navigator.cookieEnabled ? on() : off();
        }
    }

    // Blink (chrome & opera)
    window.webkitRequestFileSystem ? webkitRequestFileSystem(0, 0, off, on)
        // FF
        : "MozAppearance" in document.documentElement.style ? navigator.serviceWorker ? off() : on()
        // Safari
        : /constructor/i.test(window.HTMLElement) || window.safari ? tryls()
            // IE10+ & edge
            : !window.indexedDB && (window.PointerEvent || window.MSPointerEvent) ? on()
                // Rest
                : off()
}

function leoGetPosition(leowElem) {
    var rect = leowElem.getBoundingClientRect();
    // console.log(rect.top, rect.left);
    var body = document.body;
    var docEl = document.documentElement;
    var scrollTop = window.pageYOffset || docEl.scrollTop || body.scrollTop;
    var scrollLeft = window.pageXOffset || docEl.scrollLeft || body.scrollLeft;
    var clientTop = docEl.clientTop || body.clientTop || 0;
    var clientLeft = docEl.clientLeft || body.clientLeft || 0;
    var top = rect.top + scrollTop - clientTop;
    var left = rect.left + scrollLeft - clientLeft;
    var position = {
        'top': top,
        'left': left,
        'height': rect.height,
        'width': rect.width,
    };
    return position;
}

function leoSetLockPosition(generated_html, bdwrapper, index) {
    var element = document.getElementById('leo-bd-wrapper-' + index);
    if (element) element.parentNode.removeChild(element);
    let position = leoGetPosition(bdwrapper);
    document.getElementsByTagName("body")[0].insertAdjacentHTML("afterBegin", generated_html);
    let generatedHtmlDom = document.getElementById('leo-bd-wrapper-' + index);

    generatedHtmlDom.style.left = position.left + 'px';
    generatedHtmlDom.style.right = 'auto';
    generatedHtmlDom.style.top = position.top + 'px';
    generatedHtmlDom.style.height = position.height + 'px';
    generatedHtmlDom.style.width = position.width + 'px';
}

function leoTimeDiff(startTime) {
    let endTime = new Date();
    let timeDiff = endTime - startTime;
    return timeDiff;
}

//--------------------------------------------------END UTILITIES
