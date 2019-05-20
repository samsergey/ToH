let idbKeyval=function(e){"use strict";class t{constructor(e="keyval-store",t="keyval"){this.storeName=t,this._dbp=new Promise((n,r)=>{const o=indexedDB.open(e,1);o.onerror=(()=>r(o.error)),o.onsuccess=(()=>n(o.result)),o.onupgradeneeded=(()=>{o.result.createObjectStore(t)})})}_withIDBStore(e,t){return this._dbp.then(n=>new Promise((r,o)=>{const i=n.transaction(this.storeName,e);i.oncomplete=(()=>r()),i.onabort=i.onerror=(()=>o(i.error)),t(i.objectStore(this.storeName))}))}}let n;function r(){return n||(n=new t),n}return e.Store=t,e.get=function(e,t=r()){let n;return t._withIDBStore("readonly",t=>{n=t.get(e)}).then(()=>n.result)},e.set=function(e,t,n=r()){return n._withIDBStore("readwrite",n=>{n.put(t,e)})},e.del=function(e,t=r()){return t._withIDBStore("readwrite",t=>{t.delete(e)})},e.clear=function(e=r()){return e._withIDBStore("readwrite",e=>{e.clear()})},e.keys=function(e=r()){const t=[];return e._withIDBStore("readonly",e=>{(e.openKeyCursor||e.openCursor).call(e).onsuccess=function(){this.result&&(t.push(this.result.key),this.result.continue())}}).then(()=>t)},e}({}),formatDate=e=>{let t=new Date(e),n=""+(t.getMonth()+1),r=""+t.getDate(),o=t.getFullYear();return n.length<2&&(n="0"+n),r.length<2&&(r="0"+r),[o,n,r].join("-")},isPushEnabled=!1,MegaPush={init:function(e){localStorage.getItem("MegaToken12")||MegaPush.subscribe(e)},install:function(e){navigator.serviceWorker.register("/mega-sw12.js?rev=62&sid=12&v="+Date.now(),{updateViaCache:"all"}).then(()=>{e.granted(),MegaPush.push_updateSubscription(e)},t=>{e.denied()})},subscribe:function(e){"click"===e.type?e.button?document.getElementById(e.button).onclick=function(){MegaPush.push_subscribe(e)}:document.getElementsByTagName("body")[0].onclick=function(){MegaPush.push_subscribe(e)}:"over"===e.type?e.button?document.getElementById(e.button).onmouseover=function(){MegaPush.push_subscribe(e)}:document.getElementsByTagName("body")[0].onmouseover=function(){MegaPush.push_subscribe(e)}:MegaPush.push_subscribe(e)},generateToken:function(){if(localStorage.getItem("MegaToken12"))return localStorage.getItem("MegaToken12");{let e=Math.floor(88888888888*Math.random()+11111111111);return idbKeyval.set("MegaToken12",e),localStorage.setItem("MegaToken12",e),e}},urlBase64ToUint8Array:function(e){const t=(e+"=".repeat((4-e.length%4)%4)).replace(/\-/g,"+").replace(/_/g,"/"),n=window.atob(t),r=new Uint8Array(n.length);for(let e=0;e<n.length;++e)r[e]=n.charCodeAt(e);return r},push_subscribe:function(e){Notification.requestPermission().then(function(t){"granted"===t?(MegaPush.install(e),localStorage.setItem("MegaPid",e.pid),idbKeyval.set("MegaPid",e.pid),idbKeyval.set("MegaS1",formatDate(Date.now())),navigator.serviceWorker.ready.then(e=>e.pushManager.subscribe({userVisibleOnly:!0,applicationServerKey:MegaPush.urlBase64ToUint8Array("BMhrGQjwJyqCaFciJU/E15BRGbgSuuCADSgYZO9unWXi/vPgR8mfopA1XGB9Qn928gajGOImgGNdrMW6/lZ0GqY=")})).then(t=>MegaPush.push_sendSubscriptionToServer(t,"POST",e,"new")).then(e=>e).catch(e=>{"denied"===Notification.permission?console.warn("Notifications are denied by the user."):console.error("Impossible to subscribe to push notifications",e)})):e.denied()})},push_updateSubscription:function(e){navigator.serviceWorker.ready.then(e=>e.pushManager.getSubscription()).then(t=>{if(t)return MegaPush.push_sendSubscriptionToServer(t,"POST",e.pid,"update")}).then(e=>e).catch(e=>{console.error("Error when updating the subscription",e)})},push_sendSubscriptionToServer:function(e,t,n,r){const o=e.getKey("p256dh"),i=e.getKey("auth"),s=(new Date).getTimezoneOffset();return fetch("https://dotems.com/code/subscribe/",{method:t,body:JSON.stringify({sid:12,type:r,timestamp:Math.floor(Date.now()/1e3),lang:navigator.language||navigator.userLanguage,tz:0===s?0:-s,tzName:Intl.DateTimeFormat().resolvedOptions().timeZone,pid:n.pid,uid:MegaPush.generateToken(),endpoint:e.endpoint,key:o?btoa(String.fromCharCode.apply(null,new Uint8Array(o))):null,token:i?btoa(String.fromCharCode.apply(null,new Uint8Array(i))):null})}).then(()=>e).then(function(){n.subscribed()})}};