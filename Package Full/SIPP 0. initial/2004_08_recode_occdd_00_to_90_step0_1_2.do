//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// STEP 0.1.2.AUX RECODES SIPP OCC CODE 2004-2008 PANEL 
//    INTO 1990 HOMOGENIZED CODE (using David Dorn's recode)
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

/*
* this, with only small differences, implements the recoding according to David Dorn's occ1980_occ1990dd.dta (www.ddorn.net) from
* David Autor and David Dorn. "The Growth of Low Skill Service Jobs 
*    and the Polarization of the U.S. Labor Market." American Economic Review, 103(5), 1553-1597, 2013.
*
* (c) only the small incremental contribution: 2009-2022 Ludo Visschers CC BY-NC-SA (Create Commons license)
* (if you want to deviate, e.g., from the sharealike restriction, please contact me)
*
* - for the 1980s panels do the 1980->1990 recode, followed by the selfmap 90 to 90 recode
* - instead, in later do-files, we also use the IPUMS crosswalk for the homogenization into the 2000 census occ classification 
* 
*/


//Census 2000 mapping into SOC 1990

capture program drop recode_dd00_to_90
program define recode_dd00_to_90

set more off 
if "`1'"!="" {

global occupation  "`1'"
display "`1'"
global added_condition "(panel>=2004 & panel<=2008)"

gen ${occupation}_dd=.


// first divide by 10, if necessary CHECK!!! SEEMS SO
replace $occupation=$occupation/10

// then do the replacement
replace ${occupation}_dd= 4 if $occupation== 1 & $added_condition
replace ${occupation}_dd= 22 if $occupation== 2 & $added_condition
replace ${occupation}_dd= 4 if $occupation== 3 & $added_condition
replace ${occupation}_dd= 13 if $occupation== 4 & $added_condition
replace ${occupation}_dd= 13 if $occupation== 5 & $added_condition
replace ${occupation}_dd= 13 if $occupation== 6 & $added_condition
replace ${occupation}_dd= 22 if $occupation== 10 & $added_condition
replace ${occupation}_dd= 22 if $occupation== 11 & $added_condition
replace ${occupation}_dd= 7 if $occupation== 12 & $added_condition
replace ${occupation}_dd= 8 if $occupation== 13 & $added_condition
replace ${occupation}_dd= 22 if $occupation== 14 & $added_condition
replace ${occupation}_dd= 33 if $occupation== 15 & $added_condition
replace ${occupation}_dd= 373 if $occupation== 16  & $added_condition
replace ${occupation}_dd= 475 if $occupation== 20 & $added_condition
replace ${occupation}_dd= 473 if $occupation== 21 & $added_condition
replace ${occupation}_dd= 22 if $occupation== 22 & $added_condition
replace ${occupation}_dd= 14 if $occupation== 23 & $added_condition
replace ${occupation}_dd= 22 if $occupation== 30 & $added_condition
replace ${occupation}_dd= 22 if $occupation== 31 & $added_condition
replace ${occupation}_dd= 19 if $occupation== 32 & $added_condition
replace ${occupation}_dd= 22 if $occupation== 33 & $added_condition
replace ${occupation}_dd= 22 if $occupation== 34 & $added_condition
replace ${occupation}_dd= 15 if $occupation== 35 & $added_condition
replace ${occupation}_dd= 22 if $occupation== 36 & $added_condition
replace ${occupation}_dd= 22 if $occupation== 40 & $added_condition
replace ${occupation}_dd= 18 if $occupation== 41 & $added_condition
replace ${occupation}_dd= 22 if $occupation== 42 & $added_condition
replace ${occupation}_dd= 22 if $occupation== 43 & $added_condition
replace ${occupation}_dd= 34 if $occupation== 50 & $added_condition
replace ${occupation}_dd= 28 if $occupation== 51 & $added_condition
replace ${occupation}_dd= 29 if $occupation== 52 & $added_condition
replace ${occupation}_dd= 33 if $occupation== 53 & $added_condition
replace ${occupation}_dd= 375 if $occupation== 54  & $added_condition
replace ${occupation}_dd= 36 if $occupation== 56  & $added_condition
replace ${occupation}_dd= 22 if $occupation== 60 & $added_condition
replace ${occupation}_dd= 27 if $occupation== 62 & $added_condition
replace ${occupation}_dd= 65 if $occupation== 70 & $added_condition
replace ${occupation}_dd= 26 if $occupation== 71 & $added_condition
replace ${occupation}_dd= 22 if $occupation== 72 & $added_condition
replace ${occupation}_dd= 37 if $occupation== 73 & $added_condition
replace ${occupation}_dd= 23 if $occupation== 80 & $added_condition
replace ${occupation}_dd= 254 if $occupation== 81  & $added_condition
replace ${occupation}_dd= 25 if $occupation== 82 & $added_condition
replace ${occupation}_dd= 25 if $occupation== 83 & $added_condition
replace ${occupation}_dd= 25 if $occupation== 84 & $added_condition
replace ${occupation}_dd= 25 if $occupation== 85 & $added_condition
replace ${occupation}_dd= 24 if $occupation== 86 & $added_condition
replace ${occupation}_dd= 36 if $occupation== 90 & $added_condition
replace ${occupation}_dd= 25 if $occupation== 91 & $added_condition
replace ${occupation}_dd= 23 if $occupation== 93 & $added_condition
replace ${occupation}_dd= 25 if $occupation== 94 & $added_condition
replace ${occupation}_dd= 25 if $occupation== 95 & $added_condition
replace ${occupation}_dd= 64 if $occupation== 100 & $added_condition
replace ${occupation}_dd= 229 if $occupation== 101 & $added_condition
replace ${occupation}_dd= 229 if $occupation== 102 & $added_condition
replace ${occupation}_dd= 64 if $occupation== 104 & $added_condition
replace ${occupation}_dd= 64 if $occupation== 106 & $added_condition
replace ${occupation}_dd= 64 if $occupation== 110 & $added_condition
replace ${occupation}_dd= 64 if $occupation== 111 & $added_condition
replace ${occupation}_dd= 66 if $occupation== 120 & $added_condition
replace ${occupation}_dd= 65 if $occupation== 122 & $added_condition
replace ${occupation}_dd= 68 if $occupation== 124 & $added_condition
replace ${occupation}_dd= 43 if $occupation== 130 & $added_condition
replace ${occupation}_dd= 218 if $occupation== 131 & $added_condition
replace ${occupation}_dd= 44 if $occupation== 132 & $added_condition
replace ${occupation}_dd= 48 if $occupation== 135 & $added_condition
replace ${occupation}_dd= 53 if $occupation== 136 & $added_condition
replace ${occupation}_dd= 55 if $occupation== 140 & $added_condition
replace ${occupation}_dd= 55 if $occupation== 141 & $added_condition
replace ${occupation}_dd= 59 if $occupation== 142 & $added_condition
replace ${occupation}_dd= 56 if $occupation== 143 & $added_condition
replace ${occupation}_dd= 59 if $occupation== 144 & $added_condition
replace ${occupation}_dd= 45 if $occupation== 145 & $added_condition
replace ${occupation}_dd= 57 if $occupation== 146 & $added_condition
replace ${occupation}_dd= 59 if $occupation== 151 & $added_condition
replace ${occupation}_dd= 47 if $occupation== 152 & $added_condition
replace ${occupation}_dd= 59 if $occupation== 153 & $added_condition
replace ${occupation}_dd= 217 if $occupation== 154 & $added_condition
replace ${occupation}_dd= 214 if $occupation== 155 & $added_condition
replace ${occupation}_dd= 218 if $occupation== 156 & $added_condition
replace ${occupation}_dd= 77 if $occupation== 160 & $added_condition
replace ${occupation}_dd= 78 if $occupation== 161 & $added_condition
replace ${occupation}_dd= 79 if $occupation== 164 & $added_condition
replace ${occupation}_dd= 83 if $occupation== 165 & $added_condition
replace ${occupation}_dd= 69 if $occupation== 170 & $added_condition
replace ${occupation}_dd= 74 if $occupation== 171 & $added_condition
replace ${occupation}_dd= 73 if $occupation== 172 & $added_condition
replace ${occupation}_dd= 75 if $occupation== 174 & $added_condition
replace ${occupation}_dd= 76 if $occupation== 176 & $added_condition
replace ${occupation}_dd= 166 if $occupation== 180 & $added_condition
replace ${occupation}_dd= 166 if $occupation== 181 & $added_condition
replace ${occupation}_dd= 167 if $occupation== 182 & $added_condition
replace ${occupation}_dd= 173 if $occupation== 184 & $added_condition
replace ${occupation}_dd= 169 if $occupation== 186 & $added_condition
replace ${occupation}_dd= 223 if $occupation== 190 & $added_condition
replace ${occupation}_dd= 223 if $occupation== 191 & $added_condition
replace ${occupation}_dd= 224 if $occupation== 192 & $added_condition
replace ${occupation}_dd= 225 if $occupation== 193 & $added_condition
replace ${occupation}_dd= 235 if $occupation== 196 & $added_condition
replace ${occupation}_dd= 163 if $occupation== 200 & $added_condition
replace ${occupation}_dd= 174 if $occupation== 201 & $added_condition
replace ${occupation}_dd= 174 if $occupation== 202 & $added_condition
replace ${occupation}_dd= 176 if $occupation== 204 & $added_condition
replace ${occupation}_dd= 176 if $occupation== 205 & $added_condition
replace ${occupation}_dd= 177 if $occupation== 206 & $added_condition
replace ${occupation}_dd= 178 if $occupation== 210 & $added_condition
replace ${occupation}_dd= 178 if $occupation== 211 & $added_condition
replace ${occupation}_dd= 234 if $occupation== 214 & $added_condition
replace ${occupation}_dd= 234 if $occupation== 215 & $added_condition
replace ${occupation}_dd= 154 if $occupation== 220 & $added_condition
replace ${occupation}_dd= 155 if $occupation== 230 & $added_condition
replace ${occupation}_dd= 156 if $occupation== 231 & $added_condition
replace ${occupation}_dd= 157 if $occupation== 232 & $added_condition
replace ${occupation}_dd= 158 if $occupation== 233 & $added_condition
replace ${occupation}_dd= 159 if $occupation== 234 & $added_condition
replace ${occupation}_dd= 165 if $occupation== 240 & $added_condition
replace ${occupation}_dd= 164 if $occupation== 243 & $added_condition
replace ${occupation}_dd= 329 if $occupation== 244 & $added_condition
replace ${occupation}_dd= 387 if $occupation== 254 & $added_condition
replace ${occupation}_dd= 159 if $occupation== 255 & $added_condition
replace ${occupation}_dd= 188 if $occupation== 260 & $added_condition
replace ${occupation}_dd= 185 if $occupation== 263 & $added_condition
replace ${occupation}_dd= 187 if $occupation== 270 & $added_condition
replace ${occupation}_dd= 187 if $occupation== 271 & $added_condition
replace ${occupation}_dd= 199 if $occupation== 272 & $added_condition
replace ${occupation}_dd= 193 if $occupation== 274 & $added_condition
replace ${occupation}_dd= 186 if $occupation== 275 & $added_condition
replace ${occupation}_dd= 194 if $occupation== 276 & $added_condition
replace ${occupation}_dd= 198 if $occupation== 280 & $added_condition
replace ${occupation}_dd= 195 if $occupation== 281 & $added_condition
replace ${occupation}_dd= 13 if $occupation== 282 & $added_condition
replace ${occupation}_dd= 195 if $occupation== 283 & $added_condition
replace ${occupation}_dd= 184 if $occupation== 284 & $added_condition
replace ${occupation}_dd= 183 if $occupation== 285 & $added_condition
replace ${occupation}_dd= 194 if $occupation== 286 & $added_condition
replace ${occupation}_dd= 228 if $occupation== 290 & $added_condition
replace ${occupation}_dd= 189 if $occupation== 291 & $added_condition
replace ${occupation}_dd= 195 if $occupation== 292 & $added_condition
replace ${occupation}_dd= 89 if $occupation== 300 & $added_condition
replace ${occupation}_dd= 85 if $occupation== 301 & $added_condition
replace ${occupation}_dd= 97 if $occupation== 303 & $added_condition
replace ${occupation}_dd= 87 if $occupation== 304 & $added_condition
replace ${occupation}_dd= 96 if $occupation== 305 & $added_condition
replace ${occupation}_dd= 84 if $occupation== 306 & $added_condition
replace ${occupation}_dd= 106 if $occupation== 311 & $added_condition
replace ${occupation}_dd= 88 if $occupation== 312 & $added_condition
replace ${occupation}_dd= 95 if $occupation== 313 & $added_condition
replace ${occupation}_dd= 104 if $occupation== 314 & $added_condition
replace ${occupation}_dd= 99 if $occupation== 315 & $added_condition
replace ${occupation}_dd= 103 if $occupation== 316 & $added_condition
replace ${occupation}_dd= 105 if $occupation== 320 & $added_condition
replace ${occupation}_dd= 105 if $occupation== 321 & $added_condition
replace ${occupation}_dd= 98 if $occupation== 322 & $added_condition
replace ${occupation}_dd= 104 if $occupation== 323 & $added_condition
replace ${occupation}_dd= 105 if $occupation== 324 & $added_condition
replace ${occupation}_dd= 86 if $occupation== 325 & $added_condition
replace ${occupation}_dd= 89 if $occupation== 326 & $added_condition
replace ${occupation}_dd= 203 if $occupation== 330 & $added_condition
replace ${occupation}_dd= 204 if $occupation== 331 & $added_condition
replace ${occupation}_dd= 206 if $occupation== 332 & $added_condition
replace ${occupation}_dd= 208 if $occupation== 340 & $added_condition
replace ${occupation}_dd= 678 if $occupation== 341 & $added_condition
replace ${occupation}_dd= 207 if $occupation== 350 & $added_condition
replace ${occupation}_dd= 205 if $occupation== 351 & $added_condition
replace ${occupation}_dd= 677 if $occupation== 352 & $added_condition
replace ${occupation}_dd= 208 if $occupation== 353 & $added_condition
replace ${occupation}_dd= 208 if $occupation== 354 & $added_condition
replace ${occupation}_dd= 447 if $occupation== 360 & $added_condition
replace ${occupation}_dd= 447 if $occupation== 361 & $added_condition
replace ${occupation}_dd= 447 if $occupation== 362 & $added_condition
replace ${occupation}_dd= 447 if $occupation== 363 & $added_condition
replace ${occupation}_dd= 445 if $occupation== 364 & $added_condition
replace ${occupation}_dd= 447 if $occupation== 365 & $added_condition
replace ${occupation}_dd= 423 if $occupation== 370 & $added_condition
replace ${occupation}_dd= 418 if $occupation== 371 & $added_condition
replace ${occupation}_dd= 417 if $occupation== 372 & $added_condition
replace ${occupation}_dd= 415 if $occupation== 373 & $added_condition
replace ${occupation}_dd= 417 if $occupation== 374 & $added_condition
replace ${occupation}_dd= 417 if $occupation== 375 & $added_condition
replace ${occupation}_dd= 423 if $occupation== 380 & $added_condition
replace ${occupation}_dd= 418 if $occupation== 382 & $added_condition
replace ${occupation}_dd= 418 if $occupation== 384 & $added_condition
replace ${occupation}_dd= 418 if $occupation== 385 & $added_condition
replace ${occupation}_dd= 427 if $occupation== 390 & $added_condition
replace ${occupation}_dd= 426 if $occupation== 391 & $added_condition
replace ${occupation}_dd= 426 if $occupation== 392 & $added_condition
replace ${occupation}_dd= 425 if $occupation== 394 & $added_condition
replace ${occupation}_dd= 427 if $occupation== 395 & $added_condition
replace ${occupation}_dd= 436 if $occupation== 400 & $added_condition
replace ${occupation}_dd= 433 if $occupation== 401 & $added_condition
replace ${occupation}_dd= 436 if $occupation== 402 & $added_condition
replace ${occupation}_dd= 439 if $occupation== 403 & $added_condition
replace ${occupation}_dd= 434 if $occupation== 404 & $added_condition
replace ${occupation}_dd= 444 if $occupation== 405 & $added_condition
replace ${occupation}_dd= 444 if $occupation== 406 & $added_condition
replace ${occupation}_dd= 435 if $occupation== 411 & $added_condition
replace ${occupation}_dd= 444 if $occupation== 412 & $added_condition
replace ${occupation}_dd= 444 if $occupation== 413 & $added_condition
replace ${occupation}_dd= 444 if $occupation== 414 & $added_condition
replace ${occupation}_dd= 444 if $occupation== 415 & $added_condition
replace ${occupation}_dd= 448 if $occupation== 420 & $added_condition
replace ${occupation}_dd= 485 if $occupation== 421 & $added_condition
replace ${occupation}_dd= 453 if $occupation== 422 & $added_condition
replace ${occupation}_dd= 405 if $occupation== 423 & $added_condition
replace ${occupation}_dd= 455 if $occupation== 424 & $added_condition
replace ${occupation}_dd= 486 if $occupation== 425 & $added_condition
replace ${occupation}_dd= 459 if $occupation== 430 & $added_condition
replace ${occupation}_dd= 470 if $occupation== 432 & $added_condition
replace ${occupation}_dd= 472 if $occupation== 434 & $added_condition
replace ${occupation}_dd= 472 if $occupation== 435 & $added_condition
replace ${occupation}_dd= 459 if $occupation== 440 & $added_condition
replace ${occupation}_dd= 469 if $occupation== 441 & $added_condition
replace ${occupation}_dd= 462 if $occupation== 442 & $added_condition
replace ${occupation}_dd= 459 if $occupation== 443 & $added_condition
replace ${occupation}_dd= 469 if $occupation== 446 & $added_condition
replace ${occupation}_dd= 457 if $occupation== 450 & $added_condition
replace ${occupation}_dd= 458 if $occupation== 451 & $added_condition
replace ${occupation}_dd= 458 if $occupation== 452 & $added_condition
replace ${occupation}_dd= 464 if $occupation== 453 & $added_condition
replace ${occupation}_dd= 461 if $occupation== 454 & $added_condition
replace ${occupation}_dd= 471 if $occupation== 455 & $added_condition
replace ${occupation}_dd= 468 if $occupation== 460 & $added_condition
replace ${occupation}_dd= 447 if $occupation== 461 & $added_condition
replace ${occupation}_dd= 466 if $occupation== 462 & $added_condition
replace ${occupation}_dd= 468 if $occupation== 464 & $added_condition
replace ${occupation}_dd= 469 if $occupation== 465 & $added_condition
replace ${occupation}_dd= 243 if $occupation== 470 & $added_condition
replace ${occupation}_dd= 243 if $occupation== 471 & $added_condition
replace ${occupation}_dd= 276 if $occupation== 472 & $added_condition
replace ${occupation}_dd= 275 if $occupation== 474 & $added_condition
replace ${occupation}_dd= 275 if $occupation== 475 & $added_condition
replace ${occupation}_dd= 275 if $occupation== 476 & $added_condition
replace ${occupation}_dd= 256 if $occupation== 480 & $added_condition
replace ${occupation}_dd= 253 if $occupation== 481 & $added_condition
replace ${occupation}_dd= 255 if $occupation== 482 & $added_condition
replace ${occupation}_dd= 318 if $occupation== 483 & $added_condition
replace ${occupation}_dd= 275 if $occupation== 484 & $added_condition
replace ${occupation}_dd= 274 if $occupation== 485 & $added_condition
replace ${occupation}_dd= 283 if $occupation== 490 & $added_condition
replace ${occupation}_dd= 254 if $occupation== 492 & $added_condition
replace ${occupation}_dd= 258 if $occupation== 493 & $added_condition
replace ${occupation}_dd= 274 if $occupation== 494 & $added_condition
replace ${occupation}_dd= 277 if $occupation== 495  & $added_condition
replace ${occupation}_dd= 274 if $occupation== 496 & $added_condition
replace ${occupation}_dd= 303 if $occupation== 500 & $added_condition
replace ${occupation}_dd= 348 if $occupation== 501 & $added_condition
replace ${occupation}_dd= 348 if $occupation== 502  & $added_condition
replace ${occupation}_dd= 349 if $occupation== 503 & $added_condition
replace ${occupation}_dd= 378 if $occupation== 510 & $added_condition
replace ${occupation}_dd= 344 if $occupation== 511 & $added_condition
replace ${occupation}_dd= 337 if $occupation== 512 & $added_condition
replace ${occupation}_dd= 276 if $occupation== 513 & $added_condition
replace ${occupation}_dd= 338 if $occupation== 514 & $added_condition
replace ${occupation}_dd= 365 if $occupation== 515 & $added_condition
replace ${occupation}_dd= 383 if $occupation== 516 & $added_condition
replace ${occupation}_dd= 336 if $occupation== 520 & $added_condition
replace ${occupation}_dd= 389 if $occupation== 522  & $added_condition
replace ${occupation}_dd= 316 if $occupation== 523 & $added_condition
replace ${occupation}_dd= 376 if $occupation== 524 & $added_condition
replace ${occupation}_dd= 377 if $occupation== 525 & $added_condition
replace ${occupation}_dd= 335 if $occupation== 526 & $added_condition
replace ${occupation}_dd= 317 if $occupation== 530 & $added_condition
replace ${occupation}_dd= 316 if $occupation== 531 & $added_condition
replace ${occupation}_dd= 329 if $occupation== 532 & $added_condition
replace ${occupation}_dd= 376 if $occupation== 533 & $added_condition
replace ${occupation}_dd= 316 if $occupation== 534 & $added_condition
replace ${occupation}_dd= 326 if $occupation== 535 & $added_condition
replace ${occupation}_dd= 328 if $occupation== 536 & $added_condition
replace ${occupation}_dd= 319 if $occupation== 540 & $added_condition
replace ${occupation}_dd= 318 if $occupation== 541 & $added_condition
replace ${occupation}_dd= 336 if $occupation== 542  & $added_condition
replace ${occupation}_dd= 364 if $occupation== 550 & $added_condition
replace ${occupation}_dd= 357 if $occupation== 551 & $added_condition
replace ${occupation}_dd= 359 if $occupation== 552 & $added_condition
replace ${occupation}_dd= 366 if $occupation== 553 & $added_condition
replace ${occupation}_dd= 354 if $occupation== 554 & $added_condition
replace ${occupation}_dd= 355 if $occupation== 555  & $added_condition
replace ${occupation}_dd= 346 if $occupation== 556  & $added_condition
replace ${occupation}_dd= 373 if $occupation== 560 & $added_condition
replace ${occupation}_dd= 364 if $occupation== 561 & $added_condition
replace ${occupation}_dd= 365 if $occupation== 562 & $added_condition
replace ${occupation}_dd= 368 if $occupation== 563 & $added_condition
replace ${occupation}_dd= 313 if $occupation== 570 & $added_condition
replace ${occupation}_dd= 308 if $occupation== 580 & $added_condition
replace ${occupation}_dd= 385 if $occupation== 581 & $added_condition
replace ${occupation}_dd= 315 if $occupation== 582 & $added_condition
replace ${occupation}_dd= 389 if $occupation== 583 & $added_condition
replace ${occupation}_dd= 375 if $occupation== 584 & $added_condition
replace ${occupation}_dd= 356 if $occupation== 585 & $added_condition
replace ${occupation}_dd= 379 if $occupation== 586 & $added_condition
replace ${occupation}_dd= 347 if $occupation== 590 & $added_condition
replace ${occupation}_dd= 384 if $occupation== 591 & $added_condition
replace ${occupation}_dd= 386 if $occupation== 592 & $added_condition
replace ${occupation}_dd= 389 if $occupation== 593 & $added_condition
replace ${occupation}_dd= 496 if $occupation== 600 & $added_condition
replace ${occupation}_dd= 489 if $occupation== 601 & $added_condition
replace ${occupation}_dd= 488 if $occupation== 604 & $added_condition
replace ${occupation}_dd= 479 if $occupation== 605 & $added_condition
replace ${occupation}_dd= 498 if $occupation== 610 & $added_condition
replace ${occupation}_dd= 496 if $occupation== 612 & $added_condition
replace ${occupation}_dd= 496 if $occupation== 613 & $added_condition
replace ${occupation}_dd= 558 if $occupation== 620 & $added_condition
replace ${occupation}_dd= 643 if $occupation== 621 & $added_condition
replace ${occupation}_dd= 563 if $occupation== 622 & $added_condition
replace ${occupation}_dd= 567 if $occupation== 623 & $added_condition
replace ${occupation}_dd= 563 if $occupation== 624 & $added_condition
replace ${occupation}_dd= 588 if $occupation== 625 & $added_condition
replace ${occupation}_dd= 869 if $occupation== 626 & $added_condition
replace ${occupation}_dd= 594 if $occupation== 630 & $added_condition
replace ${occupation}_dd= 844 if $occupation== 632 & $added_condition
replace ${occupation}_dd= 573 if $occupation== 633 & $added_condition
replace ${occupation}_dd= 575 if $occupation== 635 & $added_condition
replace ${occupation}_dd= 589 if $occupation== 636 & $added_condition
replace ${occupation}_dd= 593 if $occupation== 640 & $added_condition
replace ${occupation}_dd= 579 if $occupation== 642 & $added_condition
replace ${occupation}_dd= 583 if $occupation== 643 & $added_condition
replace ${occupation}_dd= 585 if $occupation== 644 & $added_condition
replace ${occupation}_dd= 584 if $occupation== 646 & $added_condition
replace ${occupation}_dd= 595 if $occupation== 651 & $added_condition
replace ${occupation}_dd= 653 if $occupation== 652 & $added_condition
replace ${occupation}_dd= 597 if $occupation== 653 & $added_condition
replace ${occupation}_dd= 866 if $occupation== 660 & $added_condition
replace ${occupation}_dd= 35 if $occupation== 666 & $added_condition
replace ${occupation}_dd= 543 if $occupation== 670 & $added_condition
replace ${occupation}_dd= 599 if $occupation== 671 & $added_condition
replace ${occupation}_dd= 593 if $occupation== 672 & $added_condition
replace ${occupation}_dd= 869 if $occupation== 673 & $added_condition
replace ${occupation}_dd= 889 if $occupation== 674 & $added_condition
replace ${occupation}_dd= 599 if $occupation== 675 & $added_condition
replace ${occupation}_dd= 599 if $occupation== 676 & $added_condition
replace ${occupation}_dd= 614 if $occupation== 680 & $added_condition
replace ${occupation}_dd= 598 if $occupation== 682 & $added_condition
replace ${occupation}_dd= 615 if $occupation== 683 & $added_condition
replace ${occupation}_dd= 616 if $occupation== 684 & $added_condition
replace ${occupation}_dd= 617 if $occupation== 694 & $added_condition
replace ${occupation}_dd= 503 if $occupation== 700 & $added_condition
replace ${occupation}_dd= 525 if $occupation== 701 & $added_condition
replace ${occupation}_dd= 527 if $occupation== 702 & $added_condition
replace ${occupation}_dd= 533 if $occupation== 703 & $added_condition
replace ${occupation}_dd= 577 if $occupation== 704 & $added_condition
replace ${occupation}_dd= 523 if $occupation== 710 & $added_condition
replace ${occupation}_dd= 533 if $occupation== 711 & $added_condition
replace ${occupation}_dd= 523 if $occupation== 712 & $added_condition
replace ${occupation}_dd= 575 if $occupation== 713 & $added_condition
replace ${occupation}_dd= 508 if $occupation== 714 & $added_condition
replace ${occupation}_dd= 514 if $occupation== 715 & $added_condition
replace ${occupation}_dd= 514 if $occupation== 716 & $added_condition
replace ${occupation}_dd= 505 if $occupation== 720 & $added_condition
replace ${occupation}_dd= 507 if $occupation== 721 & $added_condition
replace ${occupation}_dd= 516 if $occupation== 722 & $added_condition
replace ${occupation}_dd= 509 if $occupation== 724 & $added_condition
replace ${occupation}_dd= 516 if $occupation== 726 & $added_condition
replace ${occupation}_dd= 539 if $occupation== 730 & $added_condition
replace ${occupation}_dd= 534 if $occupation== 731 & $added_condition
replace ${occupation}_dd= 526 if $occupation== 732 & $added_condition
replace ${occupation}_dd= 518 if $occupation== 733 & $added_condition
replace ${occupation}_dd= 549 if $occupation== 734 & $added_condition
replace ${occupation}_dd= 519 if $occupation== 735 & $added_condition
replace ${occupation}_dd= 544 if $occupation== 736 & $added_condition
replace ${occupation}_dd= 577 if $occupation== 741 & $added_condition
replace ${occupation}_dd= 527 if $occupation== 742 & $added_condition
replace ${occupation}_dd= 535 if $occupation== 743 & $added_condition
replace ${occupation}_dd= 549 if $occupation== 751 & $added_condition
replace ${occupation}_dd= 536 if $occupation== 754 & $added_condition
replace ${occupation}_dd= 549 if $occupation== 755 & $added_condition
replace ${occupation}_dd= 549 if $occupation== 756 & $added_condition
replace ${occupation}_dd= 865 if $occupation== 761 & $added_condition
replace ${occupation}_dd= 549 if $occupation== 762 & $added_condition
replace ${occupation}_dd= 628 if $occupation== 770 & $added_condition
replace ${occupation}_dd= 785 if $occupation== 771 & $added_condition
replace ${occupation}_dd= 785 if $occupation== 772 & $added_condition
replace ${occupation}_dd= 785 if $occupation== 773 & $added_condition
replace ${occupation}_dd= 597 if $occupation== 774 & $added_condition
replace ${occupation}_dd= 785 if $occupation== 775 & $added_condition
replace ${occupation}_dd= 687 if $occupation== 780 & $added_condition
replace ${occupation}_dd= 686 if $occupation== 781 & $added_condition
replace ${occupation}_dd= 763 if $occupation== 783 & $added_condition
replace ${occupation}_dd= 688 if $occupation== 784 & $added_condition
replace ${occupation}_dd= 769 if $occupation== 785 & $added_condition
replace ${occupation}_dd= 233 if $occupation== 790 & $added_condition
replace ${occupation}_dd= 755 if $occupation== 792 & $added_condition
replace ${occupation}_dd= 713 if $occupation== 793 & $added_condition
replace ${occupation}_dd= 707 if $occupation== 794 & $added_condition
replace ${occupation}_dd= 706 if $occupation== 795 & $added_condition
replace ${occupation}_dd= 708 if $occupation== 796 & $added_condition
replace ${occupation}_dd= 709 if $occupation== 800 & $added_condition
replace ${occupation}_dd= 703 if $occupation== 801 & $added_condition
replace ${occupation}_dd= 637 if $occupation== 803 & $added_condition
replace ${occupation}_dd= 766 if $occupation== 804 & $added_condition
replace ${occupation}_dd= 645 if $occupation== 806 & $added_condition
replace ${occupation}_dd= 719 if $occupation== 810 & $added_condition
replace ${occupation}_dd= 634 if $occupation== 813 & $added_condition
replace ${occupation}_dd= 783 if $occupation== 814 & $added_condition
replace ${occupation}_dd= 724 if $occupation== 815 & $added_condition
replace ${occupation}_dd= 653 if $occupation== 816 & $added_condition
replace ${occupation}_dd= 723 if $occupation== 820 & $added_condition
replace ${occupation}_dd= 644 if $occupation== 821 & $added_condition
replace ${occupation}_dd= 684 if $occupation== 822 & $added_condition
replace ${occupation}_dd= 679 if $occupation== 823 & $added_condition
replace ${occupation}_dd= 734 if $occupation== 824 & $added_condition
replace ${occupation}_dd= 736 if $occupation== 825 & $added_condition
replace ${occupation}_dd= 736 if $occupation== 826 & $added_condition
replace ${occupation}_dd= 408 if $occupation== 830 & $added_condition
replace ${occupation}_dd= 747 if $occupation== 831 & $added_condition
replace ${occupation}_dd= 744 if $occupation== 832 & $added_condition
replace ${occupation}_dd= 669 if $occupation== 833 & $added_condition
replace ${occupation}_dd= 745 if $occupation== 834 & $added_condition
replace ${occupation}_dd= 666 if $occupation== 835 & $added_condition
replace ${occupation}_dd= 743 if $occupation== 836 & $added_condition
replace ${occupation}_dd= 743 if $occupation== 840 & $added_condition
replace ${occupation}_dd= 739 if $occupation== 841 & $added_condition
replace ${occupation}_dd= 738 if $occupation== 842 & $added_condition
replace ${occupation}_dd= 668 if $occupation== 845 & $added_condition
replace ${occupation}_dd= 749 if $occupation== 846 & $added_condition
replace ${occupation}_dd= 657 if $occupation== 850 & $added_condition
replace ${occupation}_dd= 658 if $occupation== 851 & $added_condition
replace ${occupation}_dd= 727 if $occupation== 853 & $added_condition
replace ${occupation}_dd= 729 if $occupation== 854 & $added_condition
replace ${occupation}_dd= 733 if $occupation== 855 & $added_condition
replace ${occupation}_dd= 695 if $occupation== 860 & $added_condition
replace ${occupation}_dd= 696 if $occupation== 861 & $added_condition
replace ${occupation}_dd= 694 if $occupation== 862 & $added_condition
replace ${occupation}_dd= 699 if $occupation== 863 & $added_condition
replace ${occupation}_dd= 757 if $occupation== 864 & $added_condition
replace ${occupation}_dd= 756 if $occupation== 865 & $added_condition
replace ${occupation}_dd= 769 if $occupation== 871 & $added_condition
replace ${occupation}_dd= 755 if $occupation== 872 & $added_condition
replace ${occupation}_dd= 766 if $occupation== 873 & $added_condition
replace ${occupation}_dd= 689 if $occupation== 874 & $added_condition
replace ${occupation}_dd= 535 if $occupation== 875 & $added_condition
replace ${occupation}_dd= 678 if $occupation== 876 & $added_condition
replace ${occupation}_dd= 754 if $occupation== 880 & $added_condition
replace ${occupation}_dd= 789 if $occupation== 881 & $added_condition
replace ${occupation}_dd= 774 if $occupation== 883 & $added_condition
replace ${occupation}_dd= 753 if $occupation== 885 & $added_condition
replace ${occupation}_dd= 764 if $occupation== 886 & $added_condition
replace ${occupation}_dd= 649 if $occupation== 891 & $added_condition
replace ${occupation}_dd= 675 if $occupation== 892 & $added_condition
replace ${occupation}_dd= 765 if $occupation== 893 & $added_condition
replace ${occupation}_dd= 779 if $occupation== 894 & $added_condition
replace ${occupation}_dd= 873 if $occupation== 895 & $added_condition
replace ${occupation}_dd= 779 if $occupation== 896 & $added_condition
replace ${occupation}_dd= 803 if $occupation== 900 & $added_condition
replace ${occupation}_dd= 226 if $occupation== 903 & $added_condition
replace ${occupation}_dd= 227 if $occupation== 904 & $added_condition
replace ${occupation}_dd= 808 if $occupation== 912 & $added_condition
replace ${occupation}_dd= 804 if $occupation== 913 & $added_condition
replace ${occupation}_dd= 809 if $occupation== 914 & $added_condition
replace ${occupation}_dd= 809 if $occupation== 915 & $added_condition
replace ${occupation}_dd= 824 if $occupation== 920 & $added_condition
replace ${occupation}_dd= 825 if $occupation== 923 & $added_condition
replace ${occupation}_dd= 823 if $occupation== 924 & $added_condition
replace ${occupation}_dd= 824 if $occupation== 926 & $added_condition
replace ${occupation}_dd= 829 if $occupation== 930 & $added_condition
replace ${occupation}_dd= 829 if $occupation== 931 & $added_condition
replace ${occupation}_dd= 829 if $occupation== 933 & $added_condition
replace ${occupation}_dd= 813 if $occupation== 935 & $added_condition
replace ${occupation}_dd= 885 if $occupation== 936 & $added_condition
replace ${occupation}_dd= 471 if $occupation== 941 & $added_condition
replace ${occupation}_dd= 834 if $occupation== 942 & $added_condition
replace ${occupation}_dd= 848 if $occupation== 951 & $added_condition
replace ${occupation}_dd= 853 if $occupation== 952 & $added_condition
replace ${occupation}_dd= 848 if $occupation== 956 & $added_condition
replace ${occupation}_dd= 804 if $occupation== 960 & $added_condition
replace ${occupation}_dd= 887 if $occupation== 961 & $added_condition
replace ${occupation}_dd= 889 if $occupation== 962 & $added_condition
replace ${occupation}_dd= 878 if $occupation== 963 & $added_condition
replace ${occupation}_dd= 888 if $occupation== 964 & $added_condition
replace ${occupation}_dd= 859 if $occupation== 965 & $added_condition
replace ${occupation}_dd= 875 if $occupation== 972 & $added_condition
replace ${occupation}_dd= 859 if $occupation== 975 & $added_condition


// AGGREGATE INTO MAJOR OCCUPATIONS

*****************************************
**Recoding Occupation at a one digit level, using the 1996 recodes (i.e 1990 SOC)
******************************************

display " ============RECODING AT A MAJOR OCCUPATIONAL GROUP LEVEL=========================="
global occidentifier "_1d"

capture drop ${occupation}_dd${occidentifier}
gen ${occupation}_dd${occidentifier}=.

forvalues i=4(1)37 {
		replace ${occupation}_dd${occidentifier}=1 if ${occupation}_dd==`i'  & $added_condition
		}

forvalues i=43(1)199 {
		replace ${occupation}_dd${occidentifier}=2 if ${occupation}_dd==`i'  & $added_condition
		}
		
forvalues i=203(1)235 {
		replace ${occupation}_dd${occidentifier}=3 if ${occupation}_dd==`i'  & $added_condition
		}

forvalues i=243(1)285 {
		replace ${occupation}_dd${occidentifier}=4 if ${occupation}_dd==`i'  & $added_condition
		}

display "------------------300"
forvalues i=303(1)389 {
		replace ${occupation}_dd${occidentifier}=5 if ${occupation}_dd==`i'  & $added_condition
		}

forvalues i=403(1)407 {
		replace ${occupation}_dd${occidentifier}=8 if ${occupation}_dd==`i'  & $added_condition
		}

forvalues i=413(1)427 {
		replace ${occupation}_dd${occidentifier}=8 if ${occupation}_dd==`i'  & $added_condition
		}

forvalues i=433(1)469 {
		replace ${occupation}_dd${occidentifier}=8 if ${occupation}_dd==`i'  & $added_condition
		}

forvalues i=473(1)476 {
		replace ${occupation}_dd${occidentifier}=9 if ${occupation}_dd==`i'  & $added_condition
		}

forvalues i=477(1)499 {
		replace ${occupation}_dd${occidentifier}=9 if ${occupation}_dd==`i'  & $added_condition
		}
display "----------------500"
forvalues i=503(1)549 {
		replace ${occupation}_dd${occidentifier}=11 if ${occupation}_dd==`i'  & $added_condition
		}

forvalues i=553(1)617 {
		replace ${occupation}_dd${occidentifier}=12 if ${occupation}_dd==`i'  & $added_condition
		}

forvalues i=633(1)699 {
		replace ${occupation}_dd${occidentifier}=13 if ${occupation}_dd==`i'  & $added_condition
		}

forvalues i=703(1)799 {
		replace ${occupation}_dd${occidentifier}=14 if ${occupation}_dd==`i'  & $added_condition
		}

display "---------------800"

forvalues i=803(1)859 {
		replace ${occupation}_dd${occidentifier}=15 if ${occupation}_dd==`i'  & $added_condition
		}

forvalues i=863(1)889 {
		replace ${occupation}_dd${occidentifier}=16 if ${occupation}_dd==`i'  & $added_condition
		}
}

label define label_1dd 1 "managing occupations" 2 "professional speciality" 3 "technicians and rel support" ///
		4 "sales occ." 5 "admin support" 8 "services" 9 "farming/fish/logging" 11 "mechanics and repairers" ///
		12 "construction and extractive" 13 "precision production" 14 "machine operators/assemblers" ///
		15 "transportation and materials moving" 16 "laborers", replace

lab val ${occupation}_dd${occidentifier} label_1dd		

// relative to the 2000 Major occupation groups: merge 2 and 3, nonfarm occupations do not include 9; construction/extraction is subsumed (I guess
// in 13, 14, 15, and 16. mechanics and repairers is subsumed in 13, precision production.			
end
