package edu.gu.hajo.translator.exception;

/**
 * Higher level exception (used to wrap checked exceptions)
 *
 * @author hajo
 */
public class TranslatorException extends RuntimeException {

    private static final long serialVersionUID = 1L;

    public TranslatorException() {
        super();
    }

    public TranslatorException(String message, Throwable cause,
            boolean enableSuppression, boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }

    public TranslatorException(String message, Throwable cause) {
        super(message, cause);
    }

    public TranslatorException(String message) {
        super(message);
    }

    public TranslatorException(Throwable cause) {
        super(cause);
    }

}
