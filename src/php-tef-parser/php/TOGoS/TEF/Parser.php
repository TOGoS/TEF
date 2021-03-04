<?php

// Could use TOGoS/PHPipeable for some of this stuff,
// but I worry a little bit about performance impact of all those metadata arrays.
// This code is rather inspired by that model, though.

interface TOGoS_TEF_ParserSink
{
	/** Indicate the source position of the next item */
	public function sourcePosition($filename, $lineNumber);
	public function openEntry($typeString, $idString);
	public function header($key, $value);
	public function text($text);
	public function closeEntry();
}

interface TOGoS_TEF_LineSink {
	public function sourcePosition($filename, $lineNumber);
	public function __invoke($line);
	public function close();
}

class TOGoS_TEF_Parser implements TOGoS_TEF_LineSink {
	const STATE_INITIAL = 'initial';
	const STATE_HEADERS = 'headers';
	const STATE_CONTENT = 'content';
	
	protected $state = 'none';
	
	protected $parserSinks = array();
	public function pipe(TOGoS_TEF_ParserSink $sink) {
		$this->parserSinks[] = $sink;	
	}
	
	public function sourcePosition($filename, $lineNumber) { }
	public function __invoke($line) {
		if( preg_match('/^=([^\s]*)(?:\s+(.*))?$/', $line, $bif) ) {
			if( $this->state != self::STATE_INITIAL ) {
				foreach( $this->parserSinks as $sink ) $sink->closeEntry();
			}
			$typeString = $bif[1];
			$idString = rtrim($bif[2]);
			foreach( $this->parserSinks as $sink ) $sink->openEntry($typeString, $idString);
			$this->state = self::STATE_HEADERS;
		} else if( $this->state == self::STATE_CONTENT ) {
			foreach( $this->parserSinks as $sink ) $sink->text($line);
		} else if( $this->state == self::STATE_HEADERS ) {
			if( preg_match('/^#/', $line) ) {
				// Comment line; ignore
			} else if( preg_match('/^(.+?):\s+(.*)$/', $line, $bif) ) {
				// TODO: handle multi-line headers!
				foreach( $this->parserSinks as $sink ) $sink->header($bif[1], rtrim($bif[2]));
			} else if( trim($line) == '' ) {
				$this->state = self::STATE_CONTENT;
			}
		} else {
			// Do nothing I guess!
			// Though someday may want to parse file headers, or have a separate mode for that?
		}
	}
	public function close() { }
}

class TOGoS_TEF_LinePrinter implements TOGoS_TEF_LineSink {
	public $shouldEmitSourceLocations = false;
	protected $sourceFilename;
	protected $sourceLineNumber;
	public function sourcePosition($filename, $lineNumber) {
		$this->sourceFilename = $filename;
		$this->sourceLineNumber = $lineNumber;
	}
	public function __invoke($line) {
		if( $this->shouldEmitSourceLocations ) {
			echo "# {$this->sourceFilename}:{$this->sourceLineNumber}\n";
		}
		echo $line;
	}
	public function close() { }
}

class TOGoS_TEF_LineSplitter {
	protected $lineSinks = array();
	public function pipe(TOGoS_TEF_LineSink $sink) {
		$this->lineSinks[] = $sink;	
	}
	
	protected $sourceFilename = '-';
	protected $sourceLineNumber = 1;
	public function sourcePosition($filename, $lineNumber) {
		$this->sourceFilename = $filename;
		$this->sourceLineNumber = $lineNumber;
	}

	protected $currentLine = "";
	protected function processCurrentLine() {
		if( $this->currentLine === '' ) return;
		foreach( $this->lineSinks as $sink ) {
			$sink->sourcePosition($this->sourceFilename, $this->sourceLineNumber);
			call_user_func($sink, $this->currentLine);
		}
		$this->currentLine = '';
	}
	
	public function __invoke($chars) {
		$currentPos = 0;
		while( ($nextNewlinePos = strpos($chars, "\n", $currentPos)) !== false ) {
			$nextLinePos = $nextNewlinePos+1;
			$segment = substr($chars, $currentPos, $nextLinePos-$currentPos);
			// echo "Read ".strlen($segment)."-byte segment: ".json_encode($segment)."\n";
			$this->currentLine .= $segment;
			$this->processCurrentLine();
			++$this->sourceLineNumber;
			$currentPos = $nextLinePos;
		}
		$this->currentLine .= substr($chars, $currentPos, strlen($chars)-$currentPos);
	}
	public function close() {
		$this->processCurrentLine();
		foreach( $this->lineSinks as $sink ) $sink->close();
	}
}
