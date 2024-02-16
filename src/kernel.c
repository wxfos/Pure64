void _start(void)
{
	char*p = (char*)0xa0000+800*4*1;
	for(int y =0;y<40;y++)
	for(int i=0;i<800;i++){		//64kb
		*p++ = 0xff;
		*p++ = 0xff;
		*p++ = 0xff;
		*p++ = 0;
	}
	for(int y =0;y<40;y++)
	for(int i=0;i<800;i++){
		*p++ = 0xff;
		*p++ = 0x00;
		*p++ = 0xff;
		*p++ = 0;
	}
	while(1){
		
	}
//    main();
}
