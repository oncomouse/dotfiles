;; -*- no-byte-compile: t; -*-
;;; completion/corfu/packages.el

(package! corfu :pin "b48d3017a47706198e04440cc1b3483bdf646771")
(package! cape :pin "bfde79ed440343c0dbf0f64cfe7913c1efbe3f83")
(when (modulep! +icons)
  (package! nerd-icons-corfu :pin "7077bb76fefc15aed967476406a19dc5c2500b3c"))
(when (modulep! +orderless)
  (package! orderless :pin "b24748093b00b37c3a572c4909f61c08fa27504f"))
(when (modulep! :os tty)
  (package! corfu-terminal :pin "501548c3d51f926c687e8cd838c5865ec45d03cc"))
(when (modulep! :editor snippets)
  (package! yasnippet-capf :pin "db12b55cd08b614cbba134008566e48d7faf660e"))
