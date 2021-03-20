package matcher.csv

import de.siegmar.fastcsv.reader.CsvReader
import de.siegmar.fastcsv.writer.CsvWriter

trait WithCsvReader {
  protected val csvReader: CsvReader = {
    val csvReader: CsvReader = new CsvReader()
    csvReader.setFieldSeparator('\t')
    csvReader.setSkipEmptyRows(true)
    csvReader.setTextDelimiter('"')
    csvReader
  }
}

trait WithCsvWriter {
  protected val csvWriter: CsvWriter = {
    val csvWriter: CsvWriter = new CsvWriter()
    csvWriter.setFieldSeparator('\t')
    csvWriter.setTextDelimiter('"')
    csvWriter
  }
}

trait WithCsvIO extends WithCsvReader with WithCsvWriter