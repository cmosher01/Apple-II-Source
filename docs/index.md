home
[download](https://github.com/cmosher01/Apple-II-Source/releases/latest)
[breakout](breakout.md)

---

This package contains reverse-engineered source code for
the **System Software** and **Disk Operating System Software**
for the original APPLE \]\[ and APPLE \]\[ PLUS computers from
[Apple, Inc.](https://www.apple.com/).

The source code is intended to be processed by [M4](https://www.gnu.org/software/m4/manual/m4.html)
and assembled by [xa65 Assembler](http://www.floodgap.com/retrotech/xa/).

### Apple \]\[ Library

(A private archives not affiliated with Apple, Inc.)

> #### Warning

The copyright law of the United States (title 17, United States
Code) governs the making of photocopies or other reproductions of
copyrighted material.
Under certain conditions specified in the law, libraries and
archives are authorized to furnish a photocopy or other reproduction.
One of these specific conditions is that the photocopy or reproduction
is _not to be used for any purpose other than private study,
scholarship, or research._ If a user makes a request for, or later
uses, a photocopy or reproduction for purposes in excess of _fair
use_, that user may be liable for copyright infringement.
This institution reserves the right to refuse to accept a copying 
order if, in its judgment, fulfillment of the order would involve 
violation of copyright law.

### Apple \]\[ System Software

The Apple \]\[ and Apple \]\[ plus computers came with System Software in their
ROM chips from the factory. The System Software is commonly called firmware,
or simply ROMs.

The first Apple \]\[ had the *Monitor* system firmware, as well as the
*Integer BASIC* programming language interpreter and command line shell,
and a few other miscellaneous features, such as *Sweet-16* and the *Mini Assembler*.
The Apple \]\[ plus had an updated Monitor, sometimes known as the *Autostart Monitor*,
and replaced Integer BASIC and the misc. routines with a new floating-point BASIC, called
*Applesoft BASIC*, which was written by Microsoft (in its earliest days).

### Apple \]\[ Disk Operating System Software

The Apple \]\[ Disk Operating System Software is commonly known as *DOS 3.3*.
This software is on most Apple \]\[ floppy disks, and is loaded into the computer
from the floppy during the *boot* process. The Disk \]\[ Controller peripheral
card contains some firmware that begins the boot process. The rest of DOS is
stored on the floppy disk. There were different versions of DOS released by
Apple, in two incompatible forms: the earlier with 13 sectors per disk track,
and the later, most common, format, with 16 sectors per track.

### Breakout

Breakout is the game that started it all. A version of
[*Breakout*](breakout.md)
in Integer BASIC, from the earliest Apple \]\[ reference
manual, with line-by-line annotations, is available.

### Recovery Effort

These pieces of software are not easily available today, in source or
binary form. The System Software can only be obtained in binary form, and only by
purchasing an actual Apple \]\[ through private sale (or auction). Some of
the source code was published (in printed form only) in various
reference manuals from Apple, which are also hard to come by. As for the
DOS software, version 3.3 is the most common, but is only available in
binary form, and only through private channels. The older versions of DOS
are almost impossible to find. And none of the DOS source code was ever
made available officially.

After years of tireless effort, we have been able to completely recreate all
of the System Software and DOS Software in source form. The source is (highly
commented) 6502 assembly language, intended for the
[xa65 Assembler](http://www.floodgap.com/retrotech/xa/),
although porting it to other 6502 assemblers
should not be too difficult. Included in the recovery effort was all
System Software for the Apple \]\[ and \]\[ plus, including the Monitor and
both BASIC interpreters. Also included were the Disk Operating
System Software versions 3.1, 3.2, 3.2.1, and the three different
releases of version 3.3. The (known) official *System Master* disks were recovered
as well. Also included were the (yet) older versions of Monitor
and Integer BASIC that were provided for the Apple I (one).

More details on specific versions are available
in source code comments, and in various README files that also come with
the source distributions.
