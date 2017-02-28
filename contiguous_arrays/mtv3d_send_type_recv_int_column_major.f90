PROGRAM MTV3D
!-- Column Major Order
!-- mpif90 mtv3d_send_type_recv_int_column_major.f90; mpirun -n 2 ./a.out 

      USE MPI

      IMPLICIT NONE
      
      INTEGER ::    M=6, N=4, O=3
      INTEGER ::    S=4, T=3, U=2
      INTEGER, DIMENSION(:,:,:),  ALLOCATABLE :: A, B

      INTEGER :: TYPE_INT_YZ, TYPE_INT_3D, SIZEOFINT, STATUS(MPI_STATUS_SIZE), IERR
      INTEGER :: RANK, NUMPROCS, DEST

      INTEGER :: K, J, I

      ALLOCATE  ( A (O,N,M) ) !O        N           M
      ALLOCATE  ( B (U,T,S) ) !U: rows, T: columns, S: slices / Depth
                              !K        J           I  iter vars
                              !Z        Y           X  Direction

!--   B Matrix Dimension (in X=4, Y = 3, Z=2 )

!                       Y, J, T, N
!                      ^
!                     '
!     Z,K,U,O  ^     '
!              '    + - - - -+
!              '   /         /
!              '  /         / |
!                /         /  
!               + - - - - +   |
!               |         |   +
!               |         |  /
!               + - - - - +   -----> X, I, S, M


      B = 99

!--   MPI Section

      CALL MPI_INIT(IERR)
      CALL MPI_COMM_RANK(MPI_COMM_WORLD, RANK, IERR) 
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD, NUMPROCS, IERR) 

!--   SENDRECV as MPI_INTEGER - Column Major
!     CALL MPI_SENDRECV (A(2:U+1,2:T+1,2:S+1), U*T*S, MPI_INTEGER, 0, 111,  &
!                        B(1:U  ,1:T  ,1:S),   U*T*S, MPI_INTEGER, 0, 111,  &
!                        MPI_COMM_WORLD, STATUS, IERR)

!--------------------------------------------------------------------
!--   MPI Derived Data Type
!--------------------------------------------------------------------

!--   Get size of MPI_INTEGER
      CALL MPI_TYPE_EXTENT(MPI_INTEGER, SIZEOFINT, IERR)

      ! Create a data type for YZ plane
      ! count       T = 3 elements along Y
      ! blocklength U = 2 elements in Z for each column along Y
      ! Stride      O = 3 elements in Z for each column along Y in original matrix

      CALL MPI_TYPE_VECTOR ( T, U, O,             MPI_INTEGER, TYPE_INT_YZ, IERR)

      ! Create a 3D data type that contains several slices of YZ plane
      ! count       S   = 4 elements along X
      ! blocklength 1     element of type_int_yz along each element of X
      ! Stride(Byte)O*N = 12 elements per YZ slice;
      !                   Size of memory from the first element of the sub-array
      !                   to the start of next slice. Hvector => Heterogenous


      CALL MPI_TYPE_HVECTOR( S, 1, O*N*SIZEOFINT, TYPE_INT_YZ, TYPE_INT_3D, IERR)
      CALL MPI_TYPE_FREE(TYPE_INT_YZ, IERR)
      CALL MPI_TYPE_COMMIT(TYPE_INT_3D, IERR)

      DEST = NUMPROCS-1

      IF ( RANK == 0) then

   !--   Initalize Arrays. A: Original Matrix. B: Sub-matrix
   
         DO I=1,M
            DO J=1,N
               DO K=1,O
                  A(K,J,I) = I + J*10 + K*100
               ENDDO
            ENDDO
         ENDDO
   
   !--   Fortran: Column Major order
   !     All elements along first index (K) are contiguous in memory.
   !     A(2,:,:) = 211  212  213  214  215  216
   !                221  222  223  224  225  226
   !                231  232  233  234  235  236
   !                241  242  243  244  245  246
   
   !     Print A linearly to check memory arrangement
   !DBG  PRINT *, A  
   !DBG  PRINT *, A(1,1,:)
   
         DO K = 1,O
            PRINT 1000, ((A(K,J,I), I=1,M), J=1,N)  ! Cache access not efficient
                                                    ! Order used only for print
         END DO
1000  FORMAT(' A=', /, 4(6(1X,I4.0), /))

   !--   Extract sub matrix: from 2nd to 4th row and 2nd to 4th column; Depth: 2nd to 3rd
         CALL MPI_SEND (A(2,2,2), 1, TYPE_INT_3D, DEST, 111,  &
                        MPI_COMM_WORLD, STATUS, IERR)
 

         DEALLOCATE (A)

      ELSE IF (RANK == DEST) THEN

   
   !--   SEND as MPI_TYPE_VECTOR; RECV as MPI_INTEGER
   !     Enough to specify the first element of A that needs to be sent.
   !     Count of A is 1; of TYPE_INT
    
         CALL MPI_RECV (B(1,1,1), U*T*S, MPI_INTEGER, 0, 111,  &
                        MPI_COMM_WORLD, STATUS, IERR)

   !--   Debug check: First index; contiuguos elements
   !DBG  PRINT *,B
   
         DO K = 1,U
            PRINT 1001, ((B(K,J,I),I=1,S), J=1,T)
         END DO
   1001  FORMAT(' B=', /, 3(4(1X,I4.0), /))
   
         DEALLOCATE (B)
      END IF

      CALL MPI_TYPE_FREE(TYPE_INT_3D, IERR)
      CALL MPI_FINALIZE(IERR)

END
