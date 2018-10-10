const id = x => x
const pi = Math.PI
const deg = 180/pi
const sin = Math.sin
const cos = Math.cos
const exp = Math.exp
const ln = Math.log
const log2 = Math.log2
const pow = Math.pow
const sqrt = Math.sqrt
const min = Math.min
const max = Math.max
const abs = Math.abs
const floor = Math.floor
const ceil = Math.ceil
const random = Math.random
const range = d3.range
const eulerGamma = 0.577215664901

const add = (x,y) => x + y
const sub = (x,y) => x - y
const prod = (x,y) => x * y
const div = (x,y) => x / y
const lt = (x,y) => x < y
const lte = (x,y) => x <= y
const gt = (x,y) => x > y
const gte = (x,y) => x >= y


function log(b,x) { return Math.log(x)/Math.log(b) || Math.log(b) }

function sqr(x) {return x*x}

function Heaviside(x) { return x < 0 ? 0 : 1 }

function Theta(x) { return Math.atan(100*x)/Math.PI+1/2 }

function Delta(x) { return 100/(Math.PI*(1 + 10000*x*x)) }

function gamma(x) {
    var p = [0.99999999999980993, 676.5203681218851, -1259.1392167224028,
        771.32342877765313, -176.61502916214059, 12.507343278686905,
        -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7
    ];
 
    var g = 7;
    if (x < 0.5) {
        return Math.PI / (Math.sin(Math.PI * x) * gamma(1 - x));
    }
 
    x -= 1;
    var a = p[0];
    var t = x + g + 0.5;
    for (var i = 1; i < p.length; i++) {
        a += p[i] / (x + i);
    }
 
    return Math.sqrt(2 * Math.PI) * Math.pow(t, x + 0.5) * Math.exp(-t) * a;
}

function erf(x){
    if (x < 0) return -erf(-x)
    return 2/sqrt(pi)*integrate(t => exp(-t*t),[0,x])
}

function fact(n) {
    if (n < 1) return 1
    var res = 1
    for(i=2;i<=n;i++) res*=i
    return res
}

function SimpsonIntegrate(f,a,x)
{
    var h = (x - a)/100; s = f(a)
    for(var i = a+h; i < x-h; i+=2*h)
	s += 4*f(i) + 2*f(i+h)
    s += 4*f(i)+f(x)
    return h*s/3
}

function piecewise([x1,x2],f)
{
    return x => (x < x1 || x > x2) ? 0 : f(x)
}

// step 1: a basic LUT with a few steps of Pascal's triangle
var binomials = [
    [1],
    [1,1],
    [1,2,1],
    [1,3,3,1],
    [1,4,6,4,1],
    [1,5,10,10,5,1],
    [1,6,15,20,15,6,1],
    [1,7,21,35,35,21,7,1],
    [1,8,28,54,70,54,28,8,1],
];

// step 2: a function that builds out the LUT if it needs to.
function binomial(n,k) {
    while(n >= binomials.length) {
	let s = binomials.length;
	let nextRow = [];
	nextRow[0] = 1;
	for(let i=1, prev=s-1; i<s; i++) {
            nextRow[i] = binomials[prev][i-1] + binomials[prev][i];
	}
	nextRow[s] = 1;
	binomials.push(nextRow);
    }
    return binomials[n][k];
}

C = binomial

function sum(f,x1,x2,dx)
{
    var s = 0, dx = dx || 1
    for(var x = x1; x < x2; x+=dx)
	s+=f(x)
    return s
}

function segment([x1,y1],[x2,y2])
{
    return x => (x-x1)/(x2-x1)*(y2-y1)+y1
}

function linearInterpolation(data)
{
    var n = data.length
    if (n == 0)
	return undefined
    if (n == 1)
	return x => data[0][1]
    if (n == 2)
	return segment(data[0],data[1])
    return function (x)
    {
	if (x <= data[0][0])
	    return segment(data[0],data[1])(x)
	if (x >= data[n-1][0])
	    return segment(data[n-2],data[n-1])(x)
	var i = data.position(([z,y]) => z >= x)
	return segment(data[i-1],data[i])(x)
    }
}
