
##################################################
# bytes
##################################################

class ByteArrayOutputStream
	constructor: (@size=1024) ->
		if @size <= 0
			@size = 1024
		@array = new Uint8Array(@size)
		@offset = 0

	write_byte: (b) ->
		if @offset >= @size
			@double()
		@array[@offset++] = b

	write_u16: (n) ->
		@write_byte (n >> 8) & 0xff
		@write_byte n & 0xff

	write_u32: (n) ->
		@write_byte (n >> 24) & 0xff
		@write_byte (n >> 16) & 0xff
		@write_byte (n >> 8) & 0xff
		@write_byte n & 0xff

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
# amf
##################################################

class ActionMessage
	constructor: (@version, @headers=[], @bodies=[]) ->

class MessageBody
	constructor: (@target_uri, @response_uri, @data) ->

class RemotingMessage
	@classname: 'flex.messaging.messages.RemotingMessage'
	@fields: ['source', 'operation', 'destination', 'body', 'clientId', 'messageId', 'timeToLive', 'timestamp', 'headers']
	constructor: (properties) ->
		for k, v of properties
			this.k = v

class_creator = (classname, properties) ->
	if not classname and not properties
		Object
	else
		class Object
			@classname = classname
			@fields = properties
		Object

ASObject = class_creator('', [])

type = do ->
	classToType = {}
	types = [
		"Boolean"
		"Number"
		"String"
		"Function"
		"Array"
		"Date"
		"RegExp"
		"Undefined"
		"Null"
	]
	for name in types
		classToType["[object #{name}]"] = name.toLowerCase()
	(obj) ->
		strType = Object::toString.call(obj)
		classToType[strType] or "object"

amf_typeof = (v) ->
	v.constructor

classname_of = (v) ->
	v.constructor.classname or ''

trait_of = (v) ->
	t = amf_typeof v
	classname = t.classname
	fields = t.fields
	dynamic = if fields then false else true
	externalizable = false
	new TraitInfo(classname, dynamic, externalizable, fields)

class TraitInfo
	constructor: (@classname, @dynamic, @externalizable, @properties=[]) ->

	create_factory: ->
		if @externalizable
			throw "Not Implemented: TraitInfo externalizable"
		class_creator(@classname, @properties)

	create: ->
		if not @factory
			@factory = @create_factory()
		new @factory

##################################################
# decode
##################################################
amf_types =
	amf0:
		kNumberType        : 0
		kBooleanType       : 1
		kStringType        : 2
		kObjectType        : 3
		kMovieClipType     : 4
		kNullType          : 5
		kUndefinedType     : 6
		kReferenceType     : 7
		kECMAArrayType     : 8
		kObjectEndType     : 9
		kStrictArrayType   : 10
		kDateType          : 11
		kLongStringType    : 12
		kUnsupportedType   : 13
		kRecordsetType     : 14
		kXMLObjectType     : 15
		kTypedObjectType   : 16
		kAvmPlusObjectType : 17
	amf3:
		kUndefinedType  : 0
		kNullType       : 1
		kFalseType      : 2
		kTrueType       : 3
		kIntegerType    : 4
		kDoubleType     : 5
		kStringType     : 6
		kXMLType        : 7
		kDateType       : 8
		kArrayType      : 9
		kObjectType     : 10
		kAvmPlusXmlType : 11
		kByteArrayType  : 12

class AMFInput extends ByteArrayInputStream
	constructor: (array) ->
		super array
		@reset()

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
		t = @read_byte()
		switch t
			when amf_types.amf0.kAvmPlusObjectType then @read_value = @read_value3; @read_value()
			else throw 'not implemented amf0 type: ' + t

	read_utf8: ->
		n = @read_u16()
		@read_utf8_n(n)

	read_value3: ->
		t = @read_byte()
		switch t
			when amf_types.amf3.kUndefinedType then undefined
			when amf_types.amf3.kNullType then null
			when amf_types.amf3.kFalseType then false
			when amf_types.amf3.kTrueType then true
			when amf_types.amf3.kIntegerType then @read_integer()
			when amf_types.amf3.kStringType then @read_utf8_vr()
			when amf_types.amf3.kArrayType then @read_array()
			when amf_types.amf3.kObjectType then @read_object()
			else throw 'not implemented amf3 type: ' + t

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

		object = ti.create()
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
		ti = new TraitInfo(classname, dynamic, externalizable)
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
#		if @input.offset != @input.size
#			throw "offset: #{@input.offset}, size: #{@input.size}"
		new ActionMessage(version, headers, body)

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
		@reset()

	reset: ->
		@object_table = []
		@string_table = []
		@traits_table = []
		@traits = {}
		@strings = {}

	create_trait: (v) ->
		ti = trait_of v
		@traits[ti.classname] = @traits_table.length
		@traits_table.push ti
		ti

	to_utf8: (s) ->
		# http://user1.matsumoto.ne.jp/~goma/js/utf.js
		utf8 = []
		i = 0
		idx = 0
		while i < s.length
			c = s.charCodeAt(i++)
			if c <= 0x7f
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
		utf8

	write_value: (v) ->
		@write_value0 v

	write_value0: (v) ->
		switch type v
			when 'array' then @write_byte(amf_types.amf0.kAvmPlusObjectType); @write_value = @write_value3; @write_value(v)
			when 'object' then @write_byte(amf_types.amf0.kmAvmPlusObjectType); @write_value = @write_value3; @write_value(v)
			else throw "Not Implemented: write_value0 " + type v

	write_utf8: (s) ->
		utf8 = @to_utf8 s
		@write_u16 utf8.length
		for b in utf8
			@write_byte b

	write_value3: (v) ->
		switch type v
			when 'null' then @write_byte amf_types.amf3.kNullType
			when 'number' then @write_number v
			when 'string' then @write_string v
			when 'object' then @write_object v
			when 'array' then @write_array v
			else throw "Not Implemented: write_value3 " + type v

	write_u29: (n) ->
		if n < 0x80
			@write_byte n
		else if n < 0x4000
			@write_byte ((n >> 7) & 0x7f) | 0x80
			@write_byte n & 0x7f
		else if n < 0x200000
			@write_byte (n >> 14) * 0x7f | 0x80
			@write_byte (n >> 7) * 0x7f | 0x80
			@write_byte n & 0x7f
		else if n < 0x40000000
			@write_byte (n >> 22) * 0x7f | 0x80
			@write_byte (n >> 15) * 0x7f | 0x80
			@write_byte (n >> 8) * 0x7f | 0x80
			@write_byte n & 0xff
		else
			throw "Integer out of range: " + n

	write_number: (n) ->
		if n % 1 == 0
			@write_integer n
		else
			@write_float n

	write_integer: (n) ->
		@write_byte amf_types.amf3.kIntegerType
		@write_u29 n

	write_utf8_vr: (s) ->
		i = @strings[s]
		if i?
			@write_u29 i << 1
		else
			if s
				utf8 = @to_utf8 s
				@write_u29 (utf8.length << 1) | 1
				for b in utf8
					@write_byte b
				@strings[s] = @string_table.length
				@string_table.push s
			else
				@write_u29 1

	write_string: (s) ->
		@write_byte amf_types.amf3.kStringType
		@write_utf8_vr s

	write_array: (v) ->
		@write_byte amf_types.amf3.kArrayType

		# TODO: write array by reference
		@write_u29 (v.length << 1) | 1

		@object_table.push(v)
		# TODO: write reference

		@write_utf8_vr ''
		for x in v
			@write_value x

	write_object: (v) ->
		@write_byte amf_types.amf3.kObjectType

		# TODO: write object by reference
		classname = classname_of v
		i = @traits[classname]
		if i?
			ti = @traits_table[i]
			@write_u29 (i << 2) | 0b01
		else
			ti = @create_trait v
			@write_u29 (ti.properties.length << 4) | (if ti.dynamic then 0b1000 else 0) | (if ti.externalizable then 0b100 else 0) | 0b11
			@write_utf8_vr ti.classname
			@write_utf8_vr p for p in ti.properties

		@object_table.push(v)
		# TODO: write reference

		if ti.externalizable
			throw "Not Implemented: write externalizable"
		else
			for property in ti.properties
				@write_value v[property]
			if ti.dynamic
				for k, x of v
					if k not in ti.properties
						@write_utf8_vr k
						@write_value x
				@write_utf8_vr ''

class Encoder
	constructor: (@message) ->
		@output = new AMFOutput()

	write_header: (header) ->
		throw "Not Implemented: write_header"

	write_body: (body) ->
		@output.write_utf8 body.target_uri
		@output.write_utf8 body.response_uri
		#@output.write_u32 0
		@output.write_u32 0xffffffff
		@output.reset()
		@output.write_value(body.data)

	encode: ->
		@output.write_u16(3)
		headers = @message.headers
		@output.write_u16(headers.length)
		@write_header header for header in headers
		bodies = @message.bodies
		@output.write_u16(bodies.length)
		@write_body body for body in bodies
		@output.to_array()

encode_amf = (message) ->
	encoder = new Encoder(message)
	encoder.encode()

##################################################
# connection
##################################################

class HTTPConnection
	constructor: (@url) ->

	on_post: (body, callback) ->
		xhr = new XMLHttpRequest
		xhr.open 'POST', @url, true
		xhr.responseType = 'arraybuffer'
		xhr.onload = callback
		xhr.send(body)

class AMFConnection extends HTTPConnection
	constructor: (url) ->
		super url
		@response_counter = 0

	get_response_uri: ->
		'/' + (@response_uri++)

	pack_messages: (command, args...) ->
		request_message = new ActionMessage(3)
		target_uri = command
		resposne_uri = @get_response_uri()
		amf_message = new MessageBody(target_uri, response_uri, args)
		request_message.bodies.push amf_message
		request_message

	pack_message: (destination, operation, args...) ->
		@pack_messages null, new RemotingMessage
			destination: destination
			operation: operation
			body: args

	on_message: (message, callback) ->
		bytes = encode_amf message
		@on_post message,
			(array) ->
				callback decode_amf array

	on: (destination, operation, args, callback) ->
		@on_message @pack_message(destination, operation, args...),
			(message) ->
				callback @unpack_message message

##################################################
# exports
##################################################

exports =
	ByteArrayOutputStream: ByteArrayOutputStream
	ByteArrayInputStream: ByteArrayInputStream
	decode_amf: decode_amf
	encode_amf: encode_amf

if typeof module isnt 'undefined' and module.exports
	module.exports = exports
else
	this.amf = exports


