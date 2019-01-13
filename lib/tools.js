function memoized(f)
{
    var dict = {}
    return (...x) => dict[x] || (dict[x] = f(...x))
}

function nest(f,x,n)
{
    res = x
    for(var i = 0; i < n; i++)
	res = f(res)
    return res
}

function nestList(f,x,n)
{
    res = [x]
    for(var i = 0; i < n; i++)
	res.unshift(f(res[0]))
    return res.reverse()
}


function fixedPoint(f,x,n,compare,eq) {
    if (n == 0) return x
    n = n || 100
    compare = compare || (x => x.toString())
    eq = eq || ((x,y) => x == y)
    var res = x
    for (var i = 0; i < n; i++) {
	res = f(x)
	if (eq(compare(res), compare(x))) break
	x = res
    }
    return x
}

// Array

Array.prototype.copy = function() {
    return [].concat(this)
}


Array.prototype.head = function() {
    return this[0]
}

Array.prototype.tail = function() {
    return this.slice(1,this.length)
}

Array.prototype.drop = function(n) {
    return this.slice(n,this.length)
}

Array.prototype.most = function() {
    return this.slice(0,this.length-1)
}

Array.prototype.last = function() {
    return this.slice(this.length-1,this.length)[0]
}


Array.prototype.mapappend = function(f) {
    var res = []
    for(var i = 0; i < this.length; i++)
	res = res.concat(f(this[i]))
    return res
}

Array.prototype.sorted = function(order) {
    var res = [].concat(this)
    res.sort(order || ascending)
    return res
}

var ascending = (x,y) => Number(x) - Number(y)
var descending = (x,y) => y - x

Array.prototype.sum = function(f)
{
    var ff = f || (x => x)
    return this.reduce((res,x) => res + ff(x),0)
}

Array.prototype.reversed = function()
{
    var res = [].concat(this)
    res.reverse()
    return res
}


Array.prototype.foldList = function(f,x0)
{
    var res = [], s = x0 || this[0]
    res.push(s)
    this.forEach(x => res.push(s=f(s,x)))
    return res
}

Array.prototype.accumsum = function (x0)
{
    return this.foldList((x,y)=>x+y,x0);
}

Array.prototype.ema = function (a)
{
    return this.foldList((x,y)=>x*(1-a)+a*y);
}

Array.prototype.differences = function (dx)
{
    var res = []
    for(var i = dx;i < this.length; i++)
	res.push(this[i]-this[i-dx])
    return res
}

Array.prototype.positions = function (p)
{
    var res = []
    for(var i = 0;i < this.length; i++)
	if (p(this[i])) res.push(i)
    return res
}

Array.prototype.position = function (p)
{
    for(var i = 0;i < this.length; i++)
	if (p(this[i])) return i
    return null
}

Array.prototype.find = function (p)
{
    var i = this.position(p)
    return i && this[i]
}


Array.prototype.zipWith = function(lst,f)
{
    var res = [], N = min(this.length,lst.length)
    for(var i = 0; i < N; i++)
	res.push(f(this[i], lst[i]))
    return res
}

Array.prototype.zip = function(lst)
{
    var res = [], N = min(this.length,lst.length)
    for(var i = 0; i < N; i++)
	res.push([this[i], lst[i]])
    return res
}

Array.prototype.removeAt = function(i)
{
    return this.slice(0,i).concat(this.slice(i+1,this.length))
}

Array.prototype.sample = function()
{
    return this[Math.floor(Math.random()*this.length)]
}


Array.prototype.add = function (x)
{
    var n = this.length
    if (n == 0) {
	this.push(x)
	return this
    }
    var res = [],y
    for(var i = 0; i < n; i++)
    {
	y = this.shift()
	if (y == x)
	{
	    res.push(y)
	    break
	}
	if (y > x)
	{
	    res.push(x)
	    res.push(y)
	    break
	}
	res.push(y)
    }
    if (i == n) res.push(x) 
    return res.concat(this)
}

Array.prototype.unique = function()
{
    return this.reduce((res,l) => res.add(l),[])
}


Array.prototype.merge = function(right)
{
    let result = []
    let indexLeft = 0
    let indexRight = 0
    while (indexLeft < this.length && indexRight < right.length) {
	if (this[indexLeft] < right[indexRight]) {
	    result.push(this[indexLeft])
	    indexLeft++
	} else {
	    result.push(right[indexRight])
	    indexRight++
	}
    }
    return result.concat(this.slice(indexLeft)).concat(right.slice(indexRight))
}

Array.prototype.filter = function(f)
{
    var res = []
    for(var i = 0; i < this.length; i++)
	if (f(this[i]))
	    res.push(this[i])
    return res
}

Array.prototype.repeat = function(n)
{
    return d3.range(n).mapappend(() => this)
}

Array.prototype.dot = function(v)
{
    var res = 0, i = 0
    this.forEach(x => res += x*v[i++])
    return res
}



function repeat(n,action)
{
    for(var i = 0; i < n; i++)
	action(i)
}

function transpose(a) {
    return Object.keys(a[0]).map(function(c) {
        return a.map(function(r) { return r[c]; });
    });
}

Array.prototype.transpose = function()
{
    return transpose(this.copy())
}

Array.prototype.EMA = function(alpha)
{
    var res=[]
    var s = this[0]
    res.push(s)
    for(var i=1; i < this.length-1; i++)
    {
	s = s*(1-alpha)+alpha*this[i]
	res.push(s)
    }
    return res
}


Array.prototype.partition = function(n)
{
    var res = []
    for(var i = 0; i < this.length; i+=n)
    {
	res.push([])
	for(var j = 0; j < n; j++)
	    if (i+j<this.length)
		res[i/n].push(this[i+j])
    }
    return res
}

Array.prototype.runs = function()
{
    if (this.length==0) return []
    if (this.length==1) return [this]
    var x = this[0]
    var res = [[x]]
    for(var i = 0; i < this.length; i++)
    {
	if (this[i] == x)
	    res[0].push(x)
	else
	    res.unshift([x = this[i]])
    }
    return res.reversed()
}

Array.prototype.splitBy = function(p)
{    
    var res = [[]]
    for(var i = 0; i < this.length; i++)
    {
	if (!p(this[i]))
	    res[0].push(this[i])
	else
	    res.unshift([])
    }
    return res.reversed()
}

Array.prototype.splitOn = function(el)
{
    return this.splitBy(x => x == el)
}

