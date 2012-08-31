
##################################################
# bytes
##################################################

class ByteArrayOutputStream
	constructor: (@size=1024) ->
		if @size <= 0
			@size = 1024
		@array = new Uint8Array(@size)
		@offset = 0

	append: (b) ->
		if @offset >= @size
			@double()
		@array[@offset++] = b

	double: ->
		size = @size * 2
		a = new Uint8Array(size)
		a.set(@array)
		@array = a
		@size = size

	to_array: ->
		if @offset == @size
			@array
		else
			@array.subarray(0, @offset)

	to_normal_array: ->
		a = []
		i = 0
		while i < @offset
			a[i] = @array[i]
			i++
		a

	to_string: ->

class ByteArrayInputStream
	constructor: (@array) ->
		@size = @array.length
		@offset = 0

	read_byte: ->
		@check()
		@array[@offset++]

	read_u16: ->
		n = @array[@offset] << 8 | @array[@offset + 1]
		@offset += 2
		n

	read_u32: ->
		n = @array[@offset] << 24 | @array[@offset + 1] << 16 | @array[@offset + 2] << 8 | @array[@offset + 3]
		@offset += 4
		n

	eof: ->
		@offset >= @size

	check: (n=1) ->
		if @offset + n > @size
			console.trace 'eof'
			throw 'eof'

##################################################
# decode
##################################################

class Message
	constructor: (@version, @headers=[], @bodies=[]) ->

class MessageBody
	constructor: (@target_uri, @response_uri, @data) ->

class TraitInfo
	constructor: (@classname, @dynamic, @externalizable, @count) ->
		@properties = []

class AMFInput extends ByteArrayInputStream
	constructor: (array) ->
		super array
		@string_table = []
		@object_table = []
		@traits_table = []

	reset: ->
		@string_table = []
		@object_table = []
		@traits_table = []

	read_utf8_n: (n) ->
		# http://user1.matsumoto.ne.jp/~goma/js/utf.js
		a = @array
		i = @offset
		n += i
		chars = []
		idx = 0
		while i < n
			if a[i] <= 0x7f
				chars[idx] = a[i]
			else 
				if (a[i]>>5) == 0x6
					chars[idx] = ( (a[i] & 0x1f) << 6 ) | ( a[++i] & 0x3f )
				else if (a[i]>>4) == 0xe
					chars[idx] = ( (a[i] & 0xf) << 12 ) | ( (a[++i] & 0x3f) << 6 ) | ( a[++i] & 0x3f )
				else
					s = 1
					while a[i] & (0x20 >>> s)
						s++
					chars[idx] = a[i] & (0x1f >>> s)
					while s-->=0
						chars[idx] = (chars[idx] << 6) ^ (a[++i] & 0x3f)
			i++
			idx++
		if i != n
			throw 'invalid utf-8'
		@offset = n
		s = ''
		s += String.fromCharCode(x) for x in chars
		s

	read_value: ->
		@read_value0()

	read_value0: ->
		kNumberType        = 0
		kBooleanType       = 1
		kStringType        = 2
		kObjectType        = 3
		kMovieClipType     = 4
		kNullType          = 5
		kUndefinedType     = 6
		kReferenceType     = 7
		kECMAArrayType     = 8
		kObjectEndType     = 9
		kStrictArrayType   = 10
		kDateType          = 11
		kLongStringType    = 12
		kUnsupportedType   = 13
		kRecordsetType     = 14
		kXMLObjectType     = 15
		kTypedObjectType   = 16
		kAvmPlusObjectType = 17
		type = @read_byte()
		switch type
			when kAvmPlusObjectType then @read_value0 = @read_value3; @read_value()
			else throw 'not implemented amf0 type: ' + type

	read_utf8: ->
		n = @read_u16()
		@read_utf8_n(n)

	read_value3: ->
		kUndefinedType  = 0
		kNullType       = 1
		kFalseType      = 2
		kTrueType       = 3
		kIntegerType    = 4
		kDoubleType     = 5
		kStringType     = 6
		kXMLType        = 7
		kDateType       = 8
		kArrayType      = 9
		kObjectType     = 10
		kAvmPlusXmlType = 11
		kByteArrayType  = 12

		type = @read_byte()
		switch type
			when kNullType then null
			when kIntegerType then @read_integer()
			when kStringType then @read_utf8_vr()
			when kArrayType then @read_array()
			when kObjectType then @read_object()
			else throw 'not implemented amf3 type: ' + type

	read_u29: ->
		b = @read_byte() & 0xff
		if b < 128
			return b

		value = (b & 0x7f) << 7
		b = @read_byte() & 0xff
		if b < 128
			return value | b

		value = (value | (b & 0x7f)) << 7
		b = @read_byte() & 0xff
		if b < 128
			return value | b

		value = (value | (b & 0x7f)) << 8
		b = @read_byte() & 0xff
		return value | b

	read_integer: ->
		i = @read_u29()
		i = (i << 3) >> 3

	read_utf8_vr: ->
		ref = @read_u29()
		if (ref & 1) == 0
			return @string_table[ref >> 1]
		n = ref >> 1
		if n == 0
			return ''
		s = @read_utf8_n(n)
		@string_table.push(s)
		s

	read_object_ref: (n) ->
		@object_table[n]

	read_array: ->
		ref = @read_u29()
		if (ref & 1) == 0
			return @read_object_ref ref >> 1
		len = ref >> 1
		array = null
		map = null
		while true
			name = @read_utf8_vr()
			if name.length == 0
				break
			if map == null
				map = {}
				array = map
				@object_table.push(array)
			value = @read_value()
			map[name] = value
		if map == null
			array = []
			@object_table.push(array)
			for  [0...len]
				array.push(@read_value())
		else
			for i in [0...len]
				item = @read_value()
				map[i] = item
		array

	read_object: ->
		ref = @read_u29()
		if (ref & 1) == 0
			return @read_object_ref ref >> 1
		ti = @read_traits(ref)

		# TODO: instantiate typed object
		object = {}
		@object_table.push(object)

		if ti.externalizable
			throw "Not Implemented: read_object externalizable"
		else
			for property in ti.properties
				object[property] = @read_value()
			if ti.dynamic
				while true
					name = @read_utf8_vr()
					if name.length == 0
						break
					object[name] = @read_value()

		object

	read_traits: (ref) ->
		if (ref & 3) == 1
			return @traits_table[ref >> 2]
		externalizable = (ref & 4) == 4
		dynamic = (ref & 8) == 8
		count = ref >> 4
		classname = @read_utf8_vr()
		ti = new TraitInfo(classname, dynamic, externalizable, count)
		@traits_table.push(ti)
		ti.properties.push(@read_utf8_vr()) for [0...count]
		ti


class Decoder
	constructor: (array) ->
		@input = new AMFInput(array)

	check_version: (version) ->
		switch version
			when 3 then
			else throw 'invalid amf version: ' + version

	decode: ->
		version = @input.read_u16()
		@check_version version
		header_count = @input.read_u16()
		headers = @read_header() for [0...header_count]
		body_count = @input.read_u16()
		body = (@read_body() for [0...body_count])
		new Message(version, headers, body)

	read_header: ->
		name = @input.read_utf8()

	read_body: ->
		target_uri = @input.read_utf8()
		response_uri = @input.read_utf8()
		body_length = @input.read_u32()
		@input.reset()
		value = @input.read_value()
		new MessageBody(target_uri, response_uri, value)


decode_amf = (array) ->
	decoder = new Decoder(array)
	decoder.decode()


##################################################
# encode
##################################################

class AMFOutput extends ByteArrayOutputStream
	constructor: (size) ->
		super size

	write_utf8: (s) ->
		# http://user1.matsumoto.ne.jp/~goma/js/utf.js
		a = @array
		utf8 = []
		i = 0
		idx = 0
		while i++ < s.length
			c = s.charCodeAt(i)
			if (c <= 0x7f)
				utf8[idx++] = c
			else if c <= 0x7ff
				utf8[idx++] = 0xc0 | (c >>> 6 )
				utf8[idx++] = 0x80 | (c & 0x3f)
			else if c <= 0xffff
				utf8[idx++] = 0xe0 | (c >>> 12 )
				utf8[idx++] = 0x80 | ((c >>> 6 ) & 0x3f)
				utf8[idx++] = 0x80 | (c & 0x3f)
			else
				j = 4
				while c >> (6*j)
					j++
				utf8[idx++] = ((0xff00 >>> j) & 0xff) | (c >>> (6*--j) )
				while (j--) 
					utf8[idx++] = 0x80 | ((c >>> (6*j)) & 0x3f)

##################################################
# exports
##################################################

exports =
	ByteArrayOutputStream: ByteArrayOutputStream
	ByteArrayInputStream: ByteArrayInputStream
	decode_amf: decode_amf

if typeof module isnt 'undefined' and module.exports
	module.exports = exports
else
	this.amf = exports


