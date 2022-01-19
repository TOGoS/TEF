//// Iterator Stuff

type AsyncIter<V> = AsyncIterator<V>|AsyncIterable<V>;

//// File Path Stuff

type FilePath = string;

function toFilePath(uri:FilePath|URL) : FilePath {
	uri = uri.toString().replaceAll('\\','/');
	if(
		/^[A-Za-z]:(?:\/|$)/.exec(uri) || //  windows path
		/^[^:]+(?:\/|$)/.exec(uri) // no scheme
	) {
		// Not a URI.
		return uri;
	}
	
	if( !uri.startsWith("file:") ) {
		throw new Error(`'${uri}' appears to be a non-'file:' URI; can't transform it to a file path`);
	}
	
	const filePath = decodeURIComponent(uri.substring(5));
	let m;
	if( (m = /^\/\/\/([A-Za-z]:.*)/.exec(filePath)) != null ) {
		// e.g. "file:///C:/blah blah blah" => "C:/blah blah blah";
		return m[1];
	} else if( (m = /^\/\/(\/.*)/.exec(filePath)) != null ) {
		// e.g. "file:///foo" -> "/foo";
		return m[1];
	} else {
		return filePath;
	}
}

function dirName(uri:FilePath|URL) {
	const filePath = toFilePath(uri);
	const lastSlash = filePath.lastIndexOf('/')
	return lastSlash == -1 ? filePath : filePath.substring(0, lastSlash);
}

//// BlobLike1

interface BlobLike1 {
	/** If this blob is backed by a file, filePath is the path to it */
	filePath? : FilePath;
	/**
	 * Return an [async] iteratable that wil provide the blob's chunks.
	 * 
	 * 3 modes of operation regarding re-use of buffers:
	 * - default (buffer = undefined) :: each chunk is a unique, immutable Uint8Array
	 * - zero-length buffer provided :: iterator may re-use a buffer, but not the one provided
	 * - non-zero-length buffer provided :: iterator may re-use a buffer, and it may be the one provided
	 * 
	 * @param {Uint8Array} [buffer] a buffer into which chunk data MAY be stored.
	 *   If undefined, each Uint8Array from the resulting iterator should be unique and immutable.
	 *   If a buffer is provided, chunks may re-use the same buffer, which MAY
	 *   be the one provided.  If the provided buffer is of length zero,
	 *   (e.g. if ALLOW_BUFFER_REUSE is passed in) then that buffer cannot be used for non-empty
	 *   chunks, and the iterator must provide its own (either unique per chunk).
	 */
	getChunkIterable(buffer?:Uint8Array) : AsyncIterable<Uint8Array>;
}

class FileBlobLike implements BlobLike1 {
	constructor(protected _filePath:FilePath) {}

	public get filePath() { return this._filePath; }

	public getChunkIterable(buffer?:Uint8Array) : AsyncIterable<Uint8Array> {
		const filePath = this._filePath;
		return {
			[Symbol.asyncIterator]: async function*() {
				const bufferReuseAllowed = buffer != undefined;
				if( buffer == undefined || buffer.length == 0 ) {
					buffer = new Uint8Array(65536);
				}
				const bufferSlice = (bufferReuseAllowed ? buffer.subarray : buffer.slice).bind(buffer);

				const reader = await Deno.open(filePath, {read:true});
				try {
					let readCount : number|null;
					while( (readCount = await reader.read(buffer)) != null ) {
						yield( bufferSlice(0, readCount) );
					}
				} finally {
					console.log(`closing ${filePath} reader`);
					reader.close();
				}
			}
		}
	}
}

////  

import { TEFPiece, parseTefPieces, parseLine, parseContentChunkAtBol, parseContentChunkNotAtBol } from './tef.ts';
import { assertEquals, assertRejects } from "https://deno.land/std@0.119.0/testing/asserts.ts";

type TEFParser = (chunks:AsyncIter<Uint8Array>) => AsyncIterable<TEFPiece>;

const exampleTefFile = "example.tef";

Deno.test("parse some shit", async () => {
	const exampleTefBlob = getExampleBlob(exampleTefFile);
		console.log("Parsing ${exampleTefFile}...");
		for await(const chunk of parseTefPieces(exampleTefBlob.getChunkIterable())) {
			console.log(chunk);
		}
		console.log("Done parsing ${exampleTefFile}!");
});

async function toTefPieceArray(iter:AsyncIterable<TEFPiece>) : Promise<TEFPiece[]> {
	const arr : TEFPiece[] = [];
	const contentBuffer = new Uint8Array(65536);
	let contentPos = 0;

	function flushContentPieces() {
		if( contentPos > 0 ) {
			const data = new Uint8Array(contentBuffer.buffer, 0, contentPos);
			arr.push({
				type: "content-chunk",
				data: data,
			});
			contentPos = 0;
		}
	}

	for await( const piece of iter ) {
		if( piece.type == "content-chunk" ) {
			contentBuffer.set(piece.data, contentPos);
		} else {
			flushContentPieces();
			arr.push(piece);
		}
	}
	flushContentPieces();

	return arr;
}


Deno.test("parseLine(ambiguous)", () => {
	assertEquals(
		parseLine(new TextEncoder().encode("foo"), false),
		{ hint: 3 }
	)
});
Deno.test("parseLine(foo + LF + bar)", () => {
	assertEquals(
		parseLine(new TextEncoder().encode("foo\nbar"), false),
		{ output: "foo", processed: 4 }
	)
});
Deno.test("parseLine(foo + EOF)", () => {
	assertEquals(
		parseLine(new TextEncoder().encode("foo"), true),
		{ output: "foo", processed: 3 }
	)
});

function getExampleBlob(name:string) {
	const exampleTefFile = dirName(toFilePath(import.meta.url))+"/_testdata/"+name;
	return new FileBlobLike(exampleTefFile);
}

function parseTefToBuffer(blob:BlobLike1, bufSize:null|number) : Promise<TEFPiece[]> {
	const buffer =
		bufSize === null ? undefined :
		new Uint8Array(bufSize);
	return toTefPieceArray(parseTefPieces(blob.getChunkIterable(buffer)));
}

Deno.test("parsing with different buffer sizes gives the same result", async () => {
	const bufSizes = [null,0,1,2,3,4,5,10,50,100,1000];

	const exampleBlob = getExampleBlob("example.tef");

	const results = bufSizes.map(bufSize => parseTefToBuffer(exampleBlob, bufSize));

	const result0 = await results[0];

	for( const resProm of results ) {
		const res = await resProm;
		assertEquals( res, result0 );
	}
});

async function ignoreItems(iterable:AsyncIterable<unknown>) {
	for await( const _item of iterable ) { /* do nothing */ }
}

function testParseError(parser:TEFParser, exampleFileName:string, expectInErrorMessage:string) {
	return assertRejects(
		() => ignoreItems(parser(getExampleBlob(exampleFileName).getChunkIterable())),
		undefined,
		expectInErrorMessage
	);
}

Deno.test("parser throws at misplaced header continuation 1", () => {
	return testParseError(parseTefPieces, "bad-header-continuation-1.tef", "header continuation");
});
Deno.test("parser throws at misplaced header continuation 2", () => {
	return testParseError(parseTefPieces, "bad-header-continuation-2.tef", "header continuation");
});