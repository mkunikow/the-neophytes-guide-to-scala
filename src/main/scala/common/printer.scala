package common

/**
 * Created by michal on 12/7/14.
 */
package object printer {
    def printHeader(header: String) = {
        val size = 80
        val seperator = "-"
        val strHeader = "\n" + seperator * size  + "\n" + header + "\n" + seperator * size + "\n"
        print(strHeader)
    }

}
