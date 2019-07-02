	program tciolink

C This is a non-functional program that merely serves to verify
C that linking against the cio library leaves no unresolved references.

	CALL CIO1ST	    	!cio1st.f
	CALL CIOD           !ciox.f
	CALL CIOIX          !ciox.f
	CALL CIOBX          !ciox.f
	CALL CIODX          !ciox.f
	CALL CIOI           !ciox.f
	CALL CIOB           !ciox.f
	CALL CIOAX          !ciox.f
	CALL CIOL           !ciox.f
	CALL CIOA           !ciox.f
	CALL CIOF           !ciox.f
	CALL CIOFX          !ciox.f
	CALL CIOS           !ciox.f
	CALL CIOX           !ciox.f
	CALL CIOLX          !ciox.f
	CALL CIOSX          !ciox.f
	CALL CIOQI          !cioqx.f
	CALL CIOQS          !cioqx.f
	CALL CIOQF          !cioqx.f
	CALL CIOQENTRY      !cioqx.f
	CALL CIOQLX         !cioqx.f
	CALL CIOQIX         !cioqx.f
	CALL CIOQFX         !cioqx.f
	CALL CIOQL          !cioqx.f
	CALL CIOQSX         !cioqx.f
	CALL CIOQCO         !cioqco.f
	CALL CIOQLU         !cioqlu.f
	CALL CIOPRM         !cioprm.f
	CALL CIODSS         !ciodss.f
	CALL CIOERL         !cioerl.f
	CALL CIOGBF         !ciogbf.f
	CALL CIOCMX         !ciocmx.f
	CALL CIO_COPY_I     !ciomov.f
	CALL CIO_COPY_D     !ciomov.f
	CALL CIO_COPY_L     !ciomov.f
	CALL CIO_COPY_S     !ciomov.f
	CALL CIOPAB         !ciopab.f
	CALL CIOMSG         !ciomsg.f
	CALL CIO_COPY_F     !ciomov.f
	CALL CIOQPU         !cioqpu.f
	CALL CIOAIF         !cioaif.f
	CALL CIOWRT         !ciowrt.f
	CALL CIOHLPINI      !ciohlp.f
	CALL CIOTBE         !ciotbe.f
	CALL CIOQSH         !cioqsh.f
	CALL CIOAIC         !cioaif.f
	CALL CIOERR         !cioerl.f
	CALL CIOYNL         !cioynl.f
	CALL CIOPUR         !ciobuf.f
	CALL CIOERS         !cioerl.f
	CALL CIOINV         !cioinv.f
	CALL CIOLUN         !ciolun.f
	CALL CIO_COPY_B     !ciomov.f
	CALL CIOLUP         !ciolup.f
	CALL CIOCMD         !ciocmd.f
	CALL CIOBUF         !ciobuf.f
	CALL CIOMOD         !ciomod.f
	CALL CIOGIT         !ciogit.f
	CALL CIOLIT         !ciolit.f
	CALL CIOHLP         !ciohlp.f
	CALL CIOLUNGET      !ciolunpool.f
	CALL CIOLUNFREE     !ciolunpool.f

	END
