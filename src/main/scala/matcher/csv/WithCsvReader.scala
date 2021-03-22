package matcher.csv

import de.siegmar.fastcsv.reader.CsvReader
import de.siegmar.fastcsv.reader.CsvReader.CsvReaderBuilder
import de.siegmar.fastcsv.writer.CsvWriter.CsvWriterBuilder
import de.siegmar.fastcsv.writer.{CsvWriter, LineDelimiter}

trait WithCsvReader {
  protected val csvReader: CsvReaderBuilder =
    CsvReader.builder()
      .fieldSeparator('\t')
      .skipEmptyRows(true)
      .errorOnDifferentFieldCount(true)

}

trait WithCsvWriter {
  protected val csvWriter: CsvWriterBuilder =
    CsvWriter.builder()
      .fieldSeparator('\t')
      .lineDelimiter(LineDelimiter.CRLF)
}

trait WithCsvIO extends WithCsvReader with WithCsvWriter