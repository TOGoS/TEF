const SANITY_CHECKS_ENABLED = true;

//// Parsing types

type SuccessfulParseResult<I,O,P> = {
	/// The output of parsing, e.g. a token or AST
	output: O,
	/// Some representation of the input that was processed, e.g. number of elements consumed
	processed: P,
};
/**
 * 'I have parsed some of the input.  Call me with the hint value as the 'hint' parameter next time.'
 * Otherwise equivalent to 'undefined'
 */
type PartialParseResult<H> = {
	hint?: H
};
type ParseResult<I,O,P,H> =
	undefined| // Not sure.  Equivalent to PartiaParseResult with no hint.  Maybe should be removed ebcause redundant.
	PartialParseResult<H>|
	false| // No match!
	SuccessfulParseResult<I,O,P>;

//// Bufferator

/** Maintains an internal buffer */
interface Bufferator<I, P> {
	/**
	 * Read into buffer until it is at least the given size or EOF is reached.
	 */
	bufferAtLeast(size?:P) : Promise<I>;
	/**
	 * Read into buffer until some condition returns non-undefined,
	 * and return its value.
	 */
	bufferUntil<X,H>(condition:(i:I,eofReached:boolean,hint?:H)=>X) : Promise<X>;
	/**
	 * Read some nonzero amount of stuff into buffer unless EOF has been reached
	 */
	bufferMore() : Promise<I>;
	skip(processed:P) : Promise<void>;
	get bufferIsEmpty() : boolean;
	get buffer() : Uint8Array;
	get eofReached(): boolean;
}

function padBufferSize(capacity:number, blockSize:number) {
	const cap = Math.ceil(capacity / blockSize) * blockSize;
	if( cap < capacity ) {
		// I don't entirely trust floating point math when it comes to calculating size_ts.
		throw new Error(`Oops, Math.ceil(${capacity} / ${blockSize}) * ${blockSize} returned ${cap}, which is < ${capacity}!`);
	}
	return cap;
}

function isSuccessfulParseResult<I,O,P,H>(o:ParseResult<I,O,P,unknown>|PartialParseResult<H>) : o is SuccessfulParseResult<I,O,P> {
	if( typeof o != 'object' ) return false;
	if( Object.hasOwn(o, 'output') ) return true;
	return false;
}

function isPartialParseResult<I,O,P,H>(o:ParseResult<I,O,P,unknown>|PartialParseResult<H>) : o is PartialParseResult<H> {
	if( typeof o != 'object' ) return false;
	if( Object.hasOwn(o, 'hint') ) return true;
	return false;
}

interface Uint8ArrayIteratorBufferatorOptions {
	/** Function to be called when bufferator is about to discard a chunk of the buffer */
	skipHandler?: (buf:Uint8Array, skipping:number) => void;
}

class Uint8ArrayIteratorBufferator implements Bufferator<Uint8Array, number> {
	#backBuf : ArrayBuffer = new ArrayBuffer(0);
	#buffer : Uint8Array = new Uint8Array(this.#backBuf, 0, 0);
	#eofReached = false;
	#source : AsyncIterator<Uint8Array>
	#skipHandler? : (buf:Uint8Array, skipping:number)=>void;
	constructor( source : AsyncIterator<Uint8Array>, opts:Uint8ArrayIteratorBufferatorOptions={} ) {
		this.#source = source;
		this.#skipHandler = opts.skipHandler;
	}
	get bufferIsEmpty() : boolean {
		return this.#buffer.length == 0;
	}
	get eofReached() : boolean {
		return this.#eofReached;
	}

	/** Ensure that the buffer uses our own #backBuf */
	#internalizeBuffer() {
		if( this.#buffer.buffer != this.#backBuf ) {
			if( this.#backBuf.byteLength < this.#buffer.length ) {
				this.#backBuf = new ArrayBuffer(padBufferSize(this.#buffer.length, 1024));
			}
			const oldBuffer = this.#buffer;
			this.#buffer = new Uint8Array(this.#backBuf, 0, oldBuffer.length);
			this.#buffer.set(oldBuffer, 0);
		}
	}

	get buffer() : Uint8Array {
		return this.#buffer;
	}

	async bufferMore() : Promise<Uint8Array> {
		if( this.eofReached ) {
			return this.#buffer;
		}
		
		// Calling source.next() might re-use an ArrayBuffer.
		// So copy anything we need to keep.
		this.#internalizeBuffer();

		const incoming = await this.#source.next();
		if( incoming.done ) {
			this.#eofReached = true;
			return this.#buffer;
		} else {
			if( this.#buffer.length == 0 ) {
				return this.#buffer = incoming.value;
			} else if( incoming.value.length == 0 ) {
				return this.#buffer;
			} else {
				if( this.#backBuf !== this.#buffer.buffer ) {
					throw new Error("Expected this.#buffer.buffer to === this.#backBuf")
				}
				const oldBuffer = this.#buffer;
				if( this.#backBuf.byteLength >= oldBuffer.byteOffset + oldBuffer.byteLength + incoming.value.byteLength ) {
					// Can append without shifting
					this.#buffer = new Uint8Array(this.#backBuf, oldBuffer.byteOffset, oldBuffer.byteLength + incoming.value.byteLength );
					this.#buffer.set(incoming.value, oldBuffer.byteLength);
				} else {
					// Need to shift things around
					if( this.#backBuf.byteLength < this.#buffer.byteLength + incoming.value.byteLength ) {
						this.#backBuf = new ArrayBuffer(padBufferSize(this.#buffer.byteLength + incoming.value.byteLength, 1024));
					}
					this.#buffer = new Uint8Array(this.#backBuf, 0, oldBuffer.byteLength + incoming.value.byteLength);
					this.#buffer.set(oldBuffer, 0);
					this.#buffer.set(incoming.value, oldBuffer.byteLength);
				}
				return this.#buffer
			}
		}
	}

	async bufferUntil<X>(cond:(buffer:Uint8Array, eofReached:boolean)=>X) : Promise<X> {
		let value : X;
		while( (value = cond(this.#buffer, this.#eofReached)) == undefined ) {
			await this.bufferMore();
		}
		return value;
	}

	async parse<I,O,F,H>(match:(buffer:Uint8Array, eofReached:boolean, hint?:H)=>ParseResult<I,O,number,H>, failValue:F, hint?:H ) : Promise<O|F> {
		while( true ) {
			const parseResult : ParseResult<I,O,number,H> = match(this.#buffer, this.#eofReached, hint);
			if( parseResult === false ) {
				return failValue;
			} else if( isSuccessfulParseResult(parseResult) ) {
				await this.skip(parseResult.processed);
				return parseResult.output;
			} else if( isPartialParseResult(parseResult) ) {
				hint = parseResult.hint;
			}
			await this.bufferMore();
		}
	}

	async bufferAtLeast(size:number) : Promise<Uint8Array> {
		while( this.#buffer.length < size && this.#eofReached ) {
			await this.bufferMore();
		}
		return this.#buffer;
	}

	skip(p:number) : Promise<void> {
		if( this.#buffer.length < p ) {
			throw new Error(`Can't skip ${p} bytes; not that much is buffered!`);
		}
		if( this.#skipHandler ) this.#skipHandler(this.#buffer, p);
		this.#buffer = this.#buffer.subarray(p);
		return Promise.resolve();
	}
}

//// Iterator utils

type AsyncIter<V> = AsyncIterator<V>|AsyncIterable<V>;

function isIterator<V>(thing:AsyncIter<V>) : thing is AsyncIterator<V> {
	return typeof((thing as AsyncIterator<V>).next) == 'function';
}

function toIterator<V>(thing:AsyncIter<V>) : AsyncIterator<V> {
	return isIterator(thing) ? thing : thing[Symbol.asyncIterator]();
}

async function* iterateAndFinally<X>(iter:AsyncIterable<X>, finalBlock:()=>Promise<void>) : AsyncIterable<X> {
	try {
		for await( const piece of iter ) yield piece;
	} finally {
		await finalBlock();
	}
}

function iterableWithFinalBlock<X>(iter:AsyncIterable<X>, finalBlock?:()=>Promise<void>) : AsyncIterable<X> {
	return finalBlock == undefined ? iter : iterateAndFinally(iter, finalBlock);
}

async function cleanUpIterator(it:AsyncIterator<unknown>) {
	if(it.return) await it.return();
}

const CHAR_LF = "\n".charCodeAt(0);
const CHAR_EQUAL = "=".charCodeAt(0);

const SEQ_LF = new Uint8Array([CHAR_LF]);
const SEQ_EMPTY = new Uint8Array([]);
const SEQ_EQUAL = new Uint8Array([CHAR_EQUAL]);
const SEQ_LF_EQUAL = new Uint8Array([CHAR_LF, CHAR_EQUAL]);


//// TEF Piece Types

export type TEFPiece = {
	type: "header",
	key: string,
	value: string,
} | {
	type: "new-entry",
	typeString: string,
	idString: string,
} | {
	type: "content-chunk",
	data: Uint8Array,
} | {
	type: "comment",
	// '# ' or '#!'
	commentPrefix: string,
	text: string,
}

//// TEF Parsing Functions

/** Only exported so it can be unit tested. */
export function parseLine(buf:Uint8Array, eofReached:boolean, alreadyMatched=0) : ParseResult<Uint8Array,string,number,number> {
	if( buf.length == 0 && eofReached ) return false;

	const nextLf = buf.indexOf(CHAR_LF, alreadyMatched);
	if( nextLf == -1 ) {
		if( SANITY_CHECKS_ENABLED ) {
			const line = new TextDecoder().decode(buf);
			if( line.indexOf("\n") != -1 ) {
				throw new Error(`ParseyTEFCunkReader#readLine: bug: Supposedly EOF-adjacent line contains a LF: ${JSON.stringify(line)}`);
			}
		}
		
		if( eofReached ) {
			return {
				output: new TextDecoder().decode(buf),
				processed: buf.length,
			};
		} else {
			return {
				hint: buf.length
			}
		}
	}

	const line = new TextDecoder().decode(buf.subarray(0, nextLf));
	if( SANITY_CHECKS_ENABLED && line.indexOf("\n") != -1 ) {
		throw new Error(`ParseyTEFCunkReader#readLine: bug: Line contains a LF: ${JSON.stringify(line)}`);
	}
	return {
		output: line,
		processed: nextLf+1,
	}
}

/** Only exported so it can be unit tested. */
export function parseContentChunkNotAtBol(buf:Uint8Array, eofReached:boolean) {
	if( buf.length == 0 ) return eofReached ? false : undefined;

	if( buf[0] == CHAR_LF ) {
		if( buf.length == 1 ) {
			return eofReached ? {
				output: SEQ_LF,
				processed: 1,
			} : undefined;
		} else if( buf[1] == CHAR_EQUAL ) {
			if( buf.length == 2 ) {
				return eofReached ? false : undefined;
			} else if( buf[2] == CHAR_EQUAL ) {
				// '\n==' decodes to '\n='
				// skip 3 chars and return two!
				return {
					output: SEQ_LF_EQUAL,
					processed: 3,
				}
			} else {
				return {
					output: SEQ_EMPTY,
					processed: 1
				}
			}
		}
	}

	let nextLf = -1;
	while( (nextLf = buf.indexOf(CHAR_LF, nextLf+1)) != -1 ) {
		if( nextLf == buf.length - 1 ) {
			return eofReached ? {
				output: buf,
				processed: buf.length,
			} : {
				output: buf.subarray(0, nextLf),
				processed: nextLf,
			}
		} else if( buf[nextLf+1] == CHAR_EQUAL ) {
			return {
				output: buf.subarray(0, nextLf),
				processed: nextLf,
			}
		}
	}
	
	return {
		output: buf,
		processed: buf.length
	}
}

/** Only exported so it can be unit tested. */
export function parseContentChunkAtBol(buf:Uint8Array, eofReached:boolean) {
	if( buf.length == 0 ) return eofReached ? false : undefined;
			
	if( buf[0] == CHAR_EQUAL ) {
		if( buf.length == 1 ) {
			return eofReached ? false : undefined;
		} else if( buf[1] == CHAR_EQUAL ) {
			return {
				output: SEQ_EQUAL,
				processed: 2,
			}
		} else {
			return false;
		}
	}

	return parseContentChunkNotAtBol(buf, eofReached);
}

//// Chunk Reader Classes

interface TEFChunkReader {
	/**
	 * Read everything up to but not including the next "\n" or EOF,
	 * Empty string is a valid return value, since some lines are empty.
	 * Or null if at end of file.
	 * 
	 * '==' and '\n==' escapes will *not* translated.
	 */
	readLine() : Promise<string|null>;
	/**
	 * Returns a chunk of content, or null if at the end of a content block.
	 * It is not necessarily an error to return empty chunks, but there is no reason for it
	 * given how content is encoded in TEF, and refraining from returning empty chunks
	 * will ensure that each call to readContentChunk() makes progress.
	 * 
	 * '==' and '\n==' escapes will be translated.
	 */
	readContentChunk() : Promise<Uint8Array|null>
	close() : Promise<void>;
}

/**
 * A chunk reader that uses tokenizer1.ts's parser pattern.
 * Bufferator is asked to continue buffering until our parse function
 * returns a result.
 */
 class ParseyTEFCunkReader implements TEFChunkReader {
	#bufferator : Uint8ArrayIteratorBufferator;
	#atBol = true; // Does our current position in the input stream correspond to the beginning of a line?
	#inputIterator : AsyncIterator<Uint8Array>;

	constructor( iter:AsyncIter<Uint8Array> ) {
		this.#inputIterator = toIterator(iter);
		this.#bufferator = new Uint8ArrayIteratorBufferator(
			this.#inputIterator,
			{
				// Any time input has been processed, we need to update #isBol
				// based on whether the last character was a newline
				skipHandler: (buf:Uint8Array, consumed:number) => {
					if( consumed > 0 ) {
						this.#atBol = buf[consumed-1] == CHAR_LF;
					}
				}
			}
		);
	}

	readLine() : Promise<string|null> {
		return this.#bufferator.parse(parseLine, null);
	}
	
	readContentChunk() : Promise<Uint8Array|null> {
		return this.#bufferator.parse(this.#atBol ? parseContentChunkAtBol : parseContentChunkNotAtBol, null);
	}

	close() : Promise<void> {
		return cleanUpIterator(this.#inputIterator);
	}
}

// TODO: A chunk reader that instead of using those parse functions,
// just calls bufferator.readByte() to get past escapes,
// and that way doesn't need to worry about buffer size over and over again.
// Code would look similar to the Parsey one, but each line would only have to be visited once.
// On the other hand, parsey is probably more efficient for the normal case of large buffers,
// since escape sequences falling on buffer boundaries will happen only rarely,
// and this way we avoid doing a bunch of extra promise chaining.

async function* tefPieces(lr:TEFChunkReader) : AsyncIterable<TEFPiece> {
	let currentKey : string|undefined = undefined;
	let currentValueLines : string[] = [];

	function* closeOutAnyHeader() : Iterable<TEFPiece> {
		if( currentKey != undefined ) {
			yield {
				type: "header",
				key: currentKey,
				value: currentValueLines.join("\n"),
			};
			currentKey = undefined;
			currentValueLines = [];
		}
	}

	function assertNoContinuedHeaderStuff() {
		if( currentKey != undefined || currentValueLines.length > 0 ) throw new Error(`tefPieces[3]: bug: headers info should have been cleared out`)
	}
	
	let line;
	let m : RegExpExecArray | null;
	
	while( (line = await lr.readLine()) != null ) {
		// If header continuation, continue the header
		if( (m = /^[ \t](.*)/.exec(line)) != null ) {
			if( currentKey == undefined ) {
				throw new Error(`Invald header continuation!`)
			}
			currentValueLines.push(m[1]);
			continue;
		}
		
		for( const h of closeOutAnyHeader() ) yield h;
		if( SANITY_CHECKS_ENABLED ) assertNoContinuedHeaderStuff();

		// Handle '==' and '=' lines:
		if( (m = /^=(=.*)$/.exec(line)) != null ) {
			line = m[1];
		} else if( (m = /^=([^\s]*)(?:\s+(.*))?$/.exec(line)) != null ) {
			yield {
				type: "new-entry",
				typeString: m[1],
				idString: m[2],
			};
			continue;
		} else {
			if( SANITY_CHECKS_ENABLED && line.startsWith("=") ) {
				throw new Error(`tefPieces[3]: bug: Somehow I got a line starting with '=' where I did not expect: '${line}'`);
			}
		}

		if( (m = /^(#[! ])(.*)$/.exec(line)) != null ) {
			yield {
				type: "comment",
				commentPrefix: m[1],
				text: m[2],
			}
			continue;
		}
		
		// Handle content delimiter
		if( /^\r*$/.exec(line) ) {
			let contentChunk;
			while( (contentChunk = await lr.readContentChunk()) != null ) {
				yield {
					type: "content-chunk",
					data: contentChunk
				};
			}
			continue;
		}
		
		// Handle header
		if( (m = /^([^:](?:[^: ]|:[^: ])+):(?:\s(.*))?$/.exec(line)) != null ) {
			currentKey = m[1];
			currentValueLines = m[2] == undefined ? [] : [m[2]];
			continue;
		}

		throw new Error(`Malformed TEF header line: '${line}'`);
	}
	for( const h of closeOutAnyHeader() ) yield h;
	if( SANITY_CHECKS_ENABLED ) assertNoContinuedHeaderStuff();
}

/**
 * Parse a chunked byte stream into TEF pieces.
 * To avoid copying, content chunks may reference the input chunks' backing buffers,
 * so should be used immediately or bcopied to another buffer,
 * especially if the input chunks re-use a buffer!
 * 
 * TODO: Make buffer reuse optional; default to copying data to be safe
 */
export function parseTefPieces(chunks:AsyncIter<Uint8Array>) : AsyncIterable<TEFPiece> {
	const chunkReader = new ParseyTEFCunkReader(chunks);
	return iterableWithFinalBlock(tefPieces(chunkReader), () => chunkReader.close());
}
