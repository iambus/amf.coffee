
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

	read_bytes: (n) ->
		@check n
		a = @array.subarray(@offset, @offset + n)
		@offset += n
		a

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

class MessageHeader
	constructor: (@name, @data) ->

class MessageBody
	constructor: (@target_uri, @response_uri, @data) ->

class RemotingMessage
	@classname: 'flex.messaging.messages.RemotingMessage'
	@fields: ['source', 'operation', 'destination', 'body', 'clientId', 'messageId', 'timeToLive', 'timestamp', 'headers']
	constructor: (properties) ->
		for k, v of properties
			@[k] = v

class ArrayCollection
	@classname: 'flex.messaging.io.ArrayCollection'
	@externalizable: true
	constructor: (@source=[])->
	read_external: (input) ->
		@source = input.read_value()
	write_external: ->
		throw "Not Implemented: ArrayCollection.write_external"

class UUIDUtils
	@from_byte_array: (a) ->
		if a.length != 16
			throw "Not Implemented: incorrect UUID buffer size"
		uuid = ''
		for b, i in a
			if i == 4 || i == 6 || i == 8 || i == 10
				uuid += '-'
			uuid += '0123456789ABCDEF'[ (b & 0xF0) >>> 4 ]
			uuid += '0123456789ABCDEF'[  b & 0x0F ]
		uuid

class AbstractMessage
	@classname: 'flex.messaging.messages.AbstractMessage'
	read_flags: (input) ->
		HAS_NEXT_FLAG = 128
		flags = []
		loop
			flag = input.read_byte() & 0xff
			flags.push flag
			unless flag & HAS_NEXT_FLAG
				break
		flags

	read_external: (input) ->
		flags_list = @read_flags input
		BODY_FLAG = 1
		CLIENT_ID_FLAG = 2
		DESTINATION_FLAG = 4
		HEADERS_FLAG = 8
		MESSAGE_ID_FLAG = 16
		TIMESTAMP_FLAG = 32
		TIME_TO_LIVE_FLAG = 64
		CLIENT_ID_BYTES_FLAG = 1
		MESSAGE_ID_BYTES_FLAG = 2
		for flags, i in flags_list
			reserved_position = 0
			if i == 0
				if (flags & BODY_FLAG) != 0
					@body = input.read_value()

				if (flags & CLIENT_ID_FLAG) != 0
					@client_id = input.read_value()

				if (flags & DESTINATION_FLAG) != 0
					@destination = input.read_value()

				if (flags & HEADERS_FLAG) != 0
					@headers = input.read_value()

				if (flags & MESSAGE_ID_FLAG) != 0
					@messageId = input.read_value()

				if (flags & TIMESTAMP_FLAG) != 0
					@timestamp = input.read_value()

				if (flags & TIME_TO_LIVE_FLAG) != 0
					@time_to_live = input.read_value()

				reserved_position = 7
			else if i == 1
				if (flags & CLIENT_ID_BYTES_FLAG) != 0
					client_id_bytes = input.read_value()
					@client_id = UUIDUtils.from_byte_array client_id_bytes

				if (flags & MESSAGE_ID_BYTES_FLAG) != 0
					message_id_bytes = input.read_value()
					@message_id = UUIDUtils.from_byte_array message_id_bytes

				reserved_position = 2
			if (flags >> reserved_position) != 0 and reserved_position < 6
				for j in [reserved_position...6]
					if ((flags >> j) & 1) != 0
						input.read_value()

class AsyncMessage extends AbstractMessage
	@classname: 'flex.messaging.messages.AsyncMessage'
	read_external: (input) ->
		super input
		flags_list = @read_flags input
		for flags, i in flags_list
			reserved_position = 0
			if i == 0
				CORRELATION_ID_FLAG = 1
				CORRELATION_ID_BYTES_FLAG = 2

				if (flags & CORRELATION_ID_FLAG) != 0
					@correlation_id = input.read_value()

				if (flags & CORRELATION_ID_BYTES_FLAG) != 0
					correlation_id_bytes = input.read_value()
					@correlation_id = UUIDUtils.from_byte_array correlation_id_bytes

				reserved_position = 2

			if (flags >> reserved_position) != 0 and reserved_position < 6
				for j in [reserved_position...6]
					if ((flags >> j) & 1) != 0
						input.read_value()

class AcknowledgeMessage extends AsyncMessage
	@classname: 'flex.messaging.messages.AcknowledgeMessage'
	read_external: (input) ->
		super input
		flags_list = @read_flags input
		for flags, i in flags_list
			reserved_position = 0

			if (flags >> reserved_position) != 0 and reserved_position < 6
				for j in [reserved_position...6]
					if ((flags >> j) & 1) != 0
						input.read_value()
	write_external: ->
		throw "Not Implemented: AcknowledgeMessage.write_external"

class AcknowledgeMessageExt extends AcknowledgeMessage
	@classname: 'flex.messaging.messages.AcknowledgeMessageExt'
	@alias: 'DSK'
	@externalizable: true

class CommandMessage extends AsyncMessage
	@classname: 'flex.messaging.messages.CommandMessage'
	read_external: (input) ->
		super input
		flags_list = @read_flags input
		for flags, i in flags_list
			reserved_position = 0

			if i == 0
				OPERATION_FLAG = 1
				if flags & OPERATION_FLAG != 0
					@operation = input.read_value()
				reserved_position = 1

			if (flags >> reserved_position) != 0 and reserved_position < 6
				for j in [reserved_position...6]
					if ((flags >> j) & 1) != 0
						input.read_value()

	write_external: ->
		throw "Not Implemented: AcknowledgeMessage.write_external"

class CommandMessageExt extends CommandMessage
	@classname: 'flex.messaging.messages.CommandMessageExt'
	@alias: 'DSC'
	@externalizable: true

class TypeFactory
	constructor: ->
		@externalizable = {}
		@classes = {}

	class_creator: (classname, dynamic, externalizable, properties) ->
		if dynamic
			if classname or properties?.length or externalizable
				throw "Not Implemented: dynamic: true, classname: #{classname}, properties: #{properties}, externalizable: #{externalizable}"
			Object
		else
			class ASObject
				@classname = classname
				@fields = properties
				@dynamic = dynamic
				@externalizable = externalizable
			ASObject

	define: (classname, dynamic, externalizable, properties) ->
		if externalizable
			throw "Can't define unkown externalizable: " + classname
		if dynamic and classname
			throw "Not Implemented: define dynamic"
		return @class_creator classname, dynamic, externalizable, properties

	lookup: (classname) ->
		@classes[classname]

	lookup_or_define: (classname, dynamic, externalizable, properties) ->
		@lookup(classname) or @define(classname, dynamic, externalizable, properties)

	register: (t) ->
		classname = t.classname
		if not classname
			throw "classname not defined: " + t
		@classes[classname] = t
		if t.externalizable
			@externalizable[classname] = t
			if t.alias
				@classes[t.alias] = @externalizable[t.alias] = t

create_default_type_factory = ->

	type_factory = new TypeFactory

	register_class = (t) -> type_factory.register t

	register_class ArrayCollection
	register_class AcknowledgeMessageExt
	register_class CommandMessageExt

	return type_factory

global_type_factory = create_default_type_factory()

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

	create: (type_factory) ->
		if not @factory
			@factory = (type_factory ? global_type_factory).lookup_or_define(@classname, @dynamic, @externalizable, @properties)
		if @factory instanceof Function
			return new @factory
		else
			return @factory.create()

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

TWOeN52 = Math.pow(2, -52)

class AMFInput extends ByteArrayInputStream
	constructor: (array, @type_factory) ->
		super array
		@reset()

	reset: ->
		@string_table = []
		@object_table = []
		@traits_table = []
		@read_value = @read_value0

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
			when amf_types.amf0.kStringType then @read_utf8()
			when amf_types.amf0.kStrictArrayType then @read_strict_array()
			when amf_types.amf0.kAvmPlusObjectType then @read_value = @read_value3; @read_value()
			else throw 'not implemented amf0 type: ' + t + ' in read_value0'

	read_utf8: ->
		n = @read_u16()
		@read_utf8_n(n)

	read_strict_array: ->
		n = @read_u32()
		a = []
		@object_table.push(a)
		a.push @read_value() for [1..n]
		a

	read_value3: ->
		t = @read_byte()
		switch t
			when amf_types.amf3.kUndefinedType then undefined
			when amf_types.amf3.kNullType then null
			when amf_types.amf3.kFalseType then false
			when amf_types.amf3.kTrueType then true
			when amf_types.amf3.kIntegerType then @read_integer()
			when amf_types.amf3.kDoubleType then @read_double()
			when amf_types.amf3.kStringType then @read_utf8_vr()
			when amf_types.amf3.kArrayType then @read_array()
			when amf_types.amf3.kObjectType then @read_object()
			when amf_types.amf3.kByteArrayType then @read_byte_array()
			else throw 'not implemented amf3 type: ' + t + ' in read_value3'

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

	read_double: ->
		# https://gist.github.com/275610
		a = @array
		i = @offset
		@offset += 8
		b1 = a[i++] & 0xff
		b2 = a[i++] & 0xff
		b3 = a[i++] & 0xff
		b4 = a[i++] & 0xff
		b5 = a[i++] & 0xff
		b6 = a[i++] & 0xff
		b7 = a[i++] & 0xff
		b8 = a[i++] & 0xff
		sign = 1 - ((b1 >> 7) << 1) 									# sign = bit 0
		exp = (((b1 << 4) & 0x7FF) | (b2 >> 4)) - 1023 					# exponent = bits 1..11

		# This crazy toString() stuff works around the fact that js ints are
		# only 32 bits and signed, giving us 31 bits to work with
		sig = (((b2 & 0xF) << 16) | (b3 << 8) | b4).toString(2) +
			((b5 >> 7) ? '1' : '0') +
			(((b5&0x7F) << 24) | (b6 << 16) | (b7 << 8) | b8).toString(2)	# significand = bits 12..63

		sig = parseInt(sig, 2)
		if (sig == 0 && exp == -1023)
			0.0
		else
			sign*(1.0 + TWOeN52*sig)*Math.pow(2, exp)

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
		loop
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

		object = ti.create @type_factory
		@object_table.push(object)

		if ti.externalizable
			object.read_external(this)
			object = object.get_value?() ? object # a trick you can convert ArrayCollection to a normal array
		else
			for property in ti.properties
				object[property] = @read_value()
			if ti.dynamic
				loop
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

	read_byte_array: ->
		ref = @read_u29()
		if (ref & 1) == 0
			return @read_object_ref ref >> 1
		len = ref >> 1
		a = @read_bytes len
		@object_table.push(a)
		a


class Decoder
	constructor: (array, type_factory) ->
		@input = new AMFInput(array, type_factory)

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
		must_understand = @input.read_byte() != 0
		header_length = @input.read_u32()
		@input.reset()
		value = @input.read_value()
		new MessageHeader(name, value)

	read_body: ->
		target_uri = @input.read_utf8()
		response_uri = @input.read_utf8()
		body_length = @input.read_u32()
		@input.reset()
		value = @input.read_value()
		new MessageBody(target_uri, response_uri, value)


decode_amf = (array, type_factory) ->
	decoder = new Decoder(array, type_factory)
	decoder.decode()


##################################################
# encode
##################################################

class AMFOutput extends ByteArrayOutputStream
	constructor: (size, @type_factory) ->
		super size
		@reset()

	reset: ->
		@object_table = []
		@string_table = []
		@traits_table = []
		@traits = {}
		@strings = {}
		@write_value = @write_value0

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
			when 'undefined' then @write_byte amf_types.amf3.kUndefinedType
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
			v.write_external this
		else
			for property in ti.properties
				@write_value v[property]
			if ti.dynamic
				for k, x of v
					if k not in ti.properties and type(k) != 'function'
						@write_utf8_vr k
						@write_value x
				@write_utf8_vr ''

class Encoder
	constructor: (@message, type_factory) ->
		@output = new AMFOutput(undefined, type_factory)

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

encode_amf = (message, type_factory) ->
	encoder = new Encoder(message, type_factory)
	encoder.encode()

##################################################
# connection
##################################################

S4 = -> (((1+Math.random())*0x10000)|0).toString(16).substring(1)
guid = -> S4()+S4()+"-"+S4()+"-"+S4()+"-"+S4()+"-"+S4()+S4()+S4()


if module?.exports?
	# HTTPConnection for node.js
	{parse} = require 'url'
	http = require 'http'
	class HTTPConnection
		constructor: (@url) ->
			{@host, @hostname, @port, @path} = parse url
			@cookies = {}

		post: (body) ->
			throw "Not Implemented: node.js"

		on_post: (body, callback) ->
			headers =
				'Content-Length': body.length
			options =
				method: 'POST'
				host: @hostname
				port: @port
				path: @path
				headers: headers
			if @content_type
				headers['Content-Type'] = @content_type
			cookie = @get_cookie()
			if cookie
				headers['Cookie'] = cookie
			request = http.request options,
				(response) =>
					cookies = response.headers['set-cookie']
					if cookies
						@update_cookie k for k in cookies
					chunks = []
					response.on 'data',
						(data) ->
							chunks.push data
					response.on 'end',
						->
							buffer = Buffer.concat chunks
							callback new Uint8Array(buffer)
			request.on 'error', (e) ->
				throw e

			request.write new Buffer body
			request.end()

		get_cookie: ->
			buffer = []
			for k, v of @cookies
				buffer.push k + '=' + v
			buffer.join('; ')

		update_cookie: (header) ->
			cookie = header.split(';')[0]
			[k, v] = cookie.split('=')
			@cookies[k] = v
else
	# HTTPConnection for browers
	class HTTPConnection
		constructor: (@url) ->

		post: (body) ->
			throw "Not Implemented: HTTPConnection.post"

		on_post: (body, callback) ->
			xhr = new XMLHttpRequest
			xhr.open 'POST', @url, true
			xhr.responseType = 'arraybuffer'
			if @content_type
				xhr.setRequestHeader('Content-Type', @content_type)
			xhr.onload = -> callback new Uint8Array xhr.response
			xhr.send body

class AMFConnection extends HTTPConnection
	constructor: (url, @type_factory) ->
		super url
		@content_type = 'application/x-amf'
		@response_counter = 0

	get_response_uri: ->
		'/' + (@response_counter++)

	pack_messages: (command, args...) ->
		request_message = new ActionMessage(3)
		target_uri = command
		response_uri = @get_response_uri()
		amf_message = new MessageBody(target_uri, response_uri, args)
		request_message.bodies.push amf_message
		request_message

	pack_message: (destination, operation, args...) ->
		@pack_messages '', new RemotingMessage
			messageId: guid()
			destination: destination
			operation: operation
			body: args

	unpack_messages: (message) ->
		message.bodies[0].data

	unpack_message: (message) ->
		message = @unpack_messages message
		classname = classname_of message
		switch classname
			when 'flex.messaging.messages.AcknowledgeMessage' then message.body
			when 'flex.messaging.messages.AcknowledgeMessageExt' then message.body
			when 'flex.messaging.messages.ErrorMessage' then @unpack_error_message(message)
			else message[0].body

	unpack_error_message: (message) ->
		s = message.faultString
		if message.faultDetail
			s += ': ' + message.faultDetail
		new Error s

	on_message: (message, callback) ->
		bytes = encode_amf message, @type_factory
		@on_post bytes,
			(array) =>
				callback decode_amf array, @type_factory

	on: (destination, operation, args, callback, errorback) ->
		@on_message @pack_message(destination, operation, args...),
			(message) =>
				result = @unpack_message message
				if result instanceof Error
					if errorback
						errorback result
					else
						throw result
				else
					callback result

	send_message: (message) ->
		bytes = encode_amf message, @type_factory
		decode_amf @post(bytes), @type_factory

	call: (destination, operation, args) ->
		@unpack_message message @send_message @pack_message(destination, operation, args...)

##################################################
# exports
##################################################

exports =
	ByteArrayOutputStream: ByteArrayOutputStream
	ByteArrayInputStream: ByteArrayInputStream
	decode_amf: decode_amf
	encode_amf: encode_amf
	AMFConnection: AMFConnection
	TypeFactory: TypeFactory
	create_default_type_factory: create_default_type_factory

if module?.exports?
	module.exports = exports
else
	this.amf = exports


