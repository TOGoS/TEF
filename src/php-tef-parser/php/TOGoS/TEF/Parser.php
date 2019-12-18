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
		$this->currentLine = "";
	}
	
	public function __invoke($chars) {
		$currentPos = 0;
		while( ($nextNewlinePos = strpos($chars, "\n", $currentPos)) !== false ) {
			$endPos = $nextNewlinePos+1;
			$segment = substr($chars, $currentPos, $endPos-$currentPos);
			// echo "Read ".strlen($segment)."-byte segment: ".json_encode($segment)."\n";
			$this->currentLine .= $segment;
			$this->processCurrentLine();
			++$this->sourceLineNumber;
			$currentPos = $endPos;
		}
		$this->currentLine .= substr($chars, $currentPos, strlen($chars)-$currentPos);
	}
	public function close() {
		$this->processCurrentLine();
		foreach( $this->lineSinks as $sink ) $sink->close();
	}
}

$linePrinter = new TOGoS_TEF_LinePrinter();
$linePrinter->shouldEmitSourceLocations = true;

$lineSplitter = new TOGoS_TEF_LineSplitter();
$lineSplitter->pipe($linePrinter);
while( ($data = fread(STDIN,10)) !== false and $data !== '' ) {
	$lineSplitter->__invoke($data);
}
$lineSplitter->close();
